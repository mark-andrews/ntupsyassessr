#' Bayesian Item Response Theory Model
#'
#' Implements a Bayesian multilevel 2PL IRT model
#'
#' @param formula A formula like score ~ person + item. The lhs is the binary
#'   score or accuracy variable. The rhs gives the person
#'   (student/respondent/subject/participant) and item (question) variables, in
#'   that order.
#' @param data The data frame with the required variables
#' @param ... Optional extra parameters for rstan, e.g. cores = 4.
#'
#' @return An rstan model
#' @export
#' @examples
#' \dontrun{
#' M <- stan_irt(score ~ nid + item, data = exam_df)
#' }
stan_irt <- function(formula, data, ...) {

  stopifnot(formula.tools::is.two.sided(formula))

  lhs_vars <- formula.tools::lhs.vars(formula)
  rhs_vars <- formula.tools::rhs.vars(formula)

  stopifnot(length(rhs_vars) == 2)
  stopifnot(length(lhs_vars) == 1)

  person_var <- rlang::sym(rhs_vars[1])
  item_var <- rlang::sym(rhs_vars[2])

  data <- dplyr::mutate(data,
                        person_uid = as.numeric(factor( {{ person_var }} )),
                        item_uid = as.numeric(factor( {{ item_var }})))

  model_data <- within(list(), {

    person <- dplyr::pull(data, person_uid)

    item <- dplyr::pull(data, item_uid)

    y <- data[[lhs_vars[1]]]

    stopifnot(all(sort(unique(y)) == c(0, 1)))

    stopifnot(length(person) == length(item))
    stopifnot(length(person) == length(y))

    N <- length(person)

    J <- length(unique(person))

    K <- length(unique(item))

  })

  M <- rstan::sampling(stanmodels$irtv1, data = model_data, ...)

  samples <- tidybayes::gather_draws(M, alpha[i], beta[i], gamma[i]) %>%
    dplyr::ungroup()

  results <- list()

  # return all alpha values along with the person_var
  results[['person']] <- dplyr::left_join(
    dplyr::select(data, {{ person_var }}, person_uid) %>%
      dplyr::distinct(),
     samples %>%
      dplyr::filter(.variable == 'alpha') %>%
      dplyr::select(person_uid = i, alpha = .value),
    by = 'person_uid'
  ) %>% dplyr::select(-person_uid)

  # return all gamma and beta values along with the item_var
  results[['item']] <- dplyr::left_join(
    dplyr::select(data, {{ item_var }}, item_uid) %>%
      dplyr::distinct(),
    samples %>%
      dplyr::filter(.variable %in% c('beta','gamma')) %>%
      dplyr::select(item_uid = i, .draw, .variable, .value),
    by = 'item_uid'
  ) %>% dplyr::select(-item_uid) %>%
    pivot_wider(names_from = .variable, values_from = .value) %>%
    select(-.draw)

  list(model = M, results = results)

}

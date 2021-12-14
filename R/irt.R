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

  model_data <- within(list(), {

    person <- as.numeric(factor(data[[rhs_vars[1]]]))

    item <- as.numeric(factor(data[[rhs_vars[2]]]))

    y <- data[[lhs_vars[1]]]

    stopifnot(all(sort(unique(y)) == c(0, 1)))

    stopifnot(length(person) == length(item))
    stopifnot(length(person) == length(y))

    N <- length(person)

    J <- length(unique(person))

    K <- length(unique(item))

  })

  rstan::sampling(stanmodels$irtv1, data = model_data, ...)

}

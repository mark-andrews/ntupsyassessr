#' Modelling Inter-marker Grading Variability
#'
#' Implements a Bayesian multilevel ordinal logistic model of grading variability.
#'
#' @param formula A formula like grade ~ marker. The lhs is the integer value of
#'  an ordinal grade (e.g. 1 represents A+, 2 represents A- ), and the rhs is
#'  an integer representing a unique grader.
#' @param data The data frame with the required variables
#' @param ... Optional extra parameters for rstan, e.g. cores = 4.
#'
#' @return A rstan model
#' @export
#' @examples
#' \dontrun{
#' M <- moderation_analysis(grade ~ marker, data = exam_df)
#' }
moderation_analysis <- function(formula, data, ...) {

  model_data <- list(
    y = data$grade_id,
    x = data$marker_id,
    N = length(data$grade_id),
    K = length(unique(data$grade_id)),
    J = length(unique(data$marker_id))
  )


  rstan::sampling(stanmodels$moderation_null_step_10_logit_2.stan, data = model_data, ...)


}

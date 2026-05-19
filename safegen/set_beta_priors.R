
#' Hyperparameter specification for Beta distribution
#'
#' Saves hyperparameters for Beta priors in named list.
#'
#' @param a,b non-negative numeric hyperparameters.
#' @param param optional character string, name of parameter for Beta distribution.
#'
#' @returns List with hyperparameters `a` and `b`.
#' @export
#'
beta_hyparam = function(a, b, param=NA_character_) {
  list(a=a, b=b, parameter=param)

}

#' Credible interval for Beta distribution
#'
#' Constructs symmetric central \eqn{100(1-\alpha)}% posterior credible
#' interval for Beta distribution with specified hyperparameters.
#'
#' @param alpha numeric in (0, 1), significance level.
#' @param a,b non-negative numeric hyperparameters.
#'
#' @returns Numeric vector with lower and upper limit of credible interval.
#' @export
#'
beta_CI = function(alpha, a, b){
  return(c(stats::qbeta(alpha/2, a, b), stats::qbeta(1-alpha/2, a, b)))
}

## To come -- prior elicitation and other prior stufff....

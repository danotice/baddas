#' Generate sample data
#'
#' Simulate sample count data for a 2x2 contingency table using the binomial distribution.
#'
#' @details
#' The study data is collated to a 2x2 contingency table:
#'      | disease | no disease | Total |
#' -----         | -----      | ------     | -----
#' test positive | \eqn{n_{11}} | \eqn{n_{12}} | \eqn{n_{1T}}
#' test negative | \eqn{n_{21}} | \eqn{n_{22}} | \eqn{n_{2T}}
#' -----         | -----      | ------     | -----
#' Total         | \eqn{n_{T1}} | \eqn{n_{T2}} | \eqn{n_{T}}
#'
#'
#' @param n total sample size.
#' @param true_prev numeric in (0,1); true value for prevalence parameter.
#' @param true_sens,true_spec numeric in (0,1); true value for sensitivity/specificity parameter,
#'    at least one of which must be given.
#' @param seed optional random seed.
#'
#' @returns Named list containing `n11`,`n22`,`nT1` and `nT2`.
#' @export
#'
#' @examples
#' generate_data_table(n=100, true_prev=0.2, true_sens=0.9, true_spec=0.8)
#'
generate_data_table = function(n, true_prev, true_sens = NA, true_spec = NA, seed = 0) {

  if (is.na(true_sens) && is.na(true_spec)){
    stop("At least one of the true parameter values for sensitivity and specificity need to be given.")
  }

  if (seed > 0) set.seed(seed)

  #data at n, simulated using true parameters
  nT1 = stats::rbinom(1, n, true_prev)
  nT2 = n - nT1

  n11 = if_else(is.na(true_sens), NA_integer_, stats::rbinom(1, nT1, true_sens))
  n22 = if_else(is.na(true_spec), NA_integer_, stats::rbinom(1, nT2, true_spec))

  return(list(nT1 = nT1, nT2 = nT2, n11 = n11, n22 = n22))
}


#' Combine contingency table data
#'
#' Merges data from two separate 2x2 contingency tables.
#'
#' @param previous_data,new_data named lists containing count data. Each list should contain
#'  names `n11, n22, nT1, nT2`.
#'
#' @returns Named lists containing count data.
#' @export
#'
#' @examples
#' samp1 = generate_data_table(n=100, true_prev=0.2, true_sens=0.9, true_spec=0.8)
#' samp2 = generate_data_table(n=48, true_prev=0.2, true_sens=0.9, true_spec=0.8)
#'
#' update_data(samp1, samp2)
#'
update_data = function(previous_data, new_data) {

  final_data = list(nT1 = previous_data$nT1 + new_data$nT1,
                    nT2 = previous_data$nT2 + new_data$nT2,
                    n11 = previous_data$n11 + new_data$n11,
                    n22 = previous_data$n22 + new_data$n22)

  return(final_data)
}

#' Determine timing of interim analyses
#'
#' Calculates the sample sizes at which interim analyses should be conducted,
#' based on a specified strategy or explicit proportions of the total
#' planned sample size.
#'
#' @param planned_n total sample size.
#' @param interims number of interim analyses (default 1). Ignored when `timing`
#'  is a vector.
#' @param timing numeric vector or character string, method to calculate timings:
#'   One of:
#'   \describe{
#'     \item{\code{"unif"}}{(default) Evenly spaces \code{interims} analyses.}
#'     \item{\code{"dec"}}{Spaces \code{interims} analyses with decreasing
#'       gaps — each successive interval is half the previous one.}
#'     \item{numeric vector}{Explicit proportions of \code{planned_n} at which
#'       to conduct analyses (e.g. \code{c(0.25, 0.5, 0.75)}). Values must all
#'       be in \eqn{(0, 1)}. Overrides `timing`.}
#'   }
#'
#' @returns A named list containing:
#'   \describe{
#'     \item{\code{planned_n}}{The total planned sample size (integer).}
#'     \item{\code{num_interim}}{The number of interim analyses (integer).}
#'     \item{\code{interims_at}}{Integer vector of sample sizes at which each
#'       interim analysis is conducted.}
#'   }
#' @export
#'
#' @examples
#' # total planned study size
#' n = 500
#'
#' # Three equally-spaced interims
#' interim_timings(n, interims = 3)
#'
#' # Two interims with decreasing spacing
#' interim_timings(n, interims = 2, timing = "dec")
#'
#' # Explicit proportions
#' interim_timings(n, timing = c(0.25, 0.5, 0.75))
#'
interim_timings = function(planned_n, interims=1, timing="") {

  # if timing is numeric vector -- proportion of planned_n
  if (is.numeric(timing)) {
    ## TODO: make error if not all < 1
    if (any(interims > 1) || any(interims <= 0)) {
      stop("invalid `timing` - numeric vector values must be between 0 and 1")
    }
    timings = sort(interims)
    interims = length(interims)
  }

  # if no interim analysis
  else if (interims == 0) {

  }

  # interims is the number of points to be evenly spaced
  else if (timing == "unif" || timing == "") {
    timings = seq(interims)/(interims + 1)
  }

  # interims is the number of points, space between analyses decreases by half
  else if (timing == "dec") {
    timings = 1 - 2^(-seq(interims))
  }

  else{
    stop("Invalid inputs.")
  }

  return(
    list(planned_n = planned_n,
         num_interim = interims,
         interims_at = if_else(interims == 0, c(), ceiling(planned_n*timings))
    )
  )

}


#' Conduct interim analysis
#'
#' Performs an interim analysis for a diagnostic accuracy study, evaluating
#' early stopping rules and re-estimating the required sample size based on
#' accumulating data.
#'
#' @inheritParams start_of_study_summary
#' @inheritParams check_study_success
#'
#' @returns Named list containing:
#'  \describe{
#'    \item{\code{additional_n}}{Estimated number of additional participants
#'       to recruit. \code{0} if stopping for efficacy, \code{NA} if stopping
#'       for futility.}
#'    \item{\code{credible_interval}}{When \code{measure} is
#'       \code{"sensitivity"} or \code{"specificity"}, a numeric vector giving
#'       the credible interval for that measure. When \code{measure} is
#'       \code{"both"}, a named list with elements \code{sens} and \code{spec},
#'       each a numeric vector giving the credible interval for the respective
#'       measure.}
#'     \item{\code{final_width}}{Width of the credible interval at the point
#'       of the interim analysis.}
#'      \item{\code{lower_assurance}}{Logical; \code{TRUE} if assurance may be
#'       below the target level at the end of study.}
#'     \item{\code{stop_for_efficacy}}{Logical; \code{TRUE} if the credible
#'       interval is already sufficiently narrow and the study can stop early.}
#'     \item{\code{stop_for_futility}}{Logical; \code{TRUE} if the target
#'       cannot be achieved within the maximum sample size.}
#'  }
#' @export
#'
#' @examples
#' int_data = generate_data_table(n=206, true_prev=0.2, true_sens=0.9, true_spec=0.8)
#' ## need to complete example
#' # check_study_success(int_data$n11, int_data$n22, int_data$nT1, int_data$nT2,
#' #  ap, "sens"
#' #  )
interim_analysis = function(n11, n22, nT1, nT2, analysis_params, measure, print=TRUE) {
  # n11 - test positive, diseased ; n22 - test negative, not disease
  # nT1 - total diseased ; nT2 - total not diseased

  # TODO - check that priors, futility assurance, target assurance and target width specified

  ap = analysis_params
  alpha = ap$alpha

  measure = valid_measure(measure)

  if (measure == "sensitivity") {
    if (is.na(n11)) stop("Number of true positives need to be given to do analysis on sensitivity.")
    output = interim_analysis_sens(n11, n22, nT1, nT2, analysis_params)
  }

  else if (measure == "specificity") {
    if(is.na(n22)) stop("Number of true negatives need to be given to do analysis on specificity.")
    output = interim_analysis_spec(n11, n22, nT1, nT2, analysis_params)
  }

  else if (measure == "both") {
    if(is.na(n11) || is.na(n22)) stop("Number of true negatives need to be given to do analysis on specificity.")
    output = interim_analysis_joint(n11, n22, nT1, nT2, analysis_params)
  }

  else {
    stop("Invalid value for `measure`.
         Please specify which measure(s) you want to calculate assurance for:
         sensitivity, specificity or both")
  }

  output['measure'] = measure

  ## end of interim analysis
  if (print) {
    if (output$stop_for_efficacy) {
      cat("Early stopping for efficacy -- credible interval has width ",
          round(output$final_width,3), ". No additional recruitment is necessary.\n", sep="")

      # additional_n = 0, final_width
    }

    else if (output$stop_for_futility) {
      cat("Early stopping for futility -- based on existing data, it will not be possible to achieve success
          without recruiting more than ",
          ap$max_n, " total participants.\n")

      # additional_n = NA
    }

    else {
      cat("Sample size re-estimated -- ", output$additional_n,
          "additional participants should be recruited. ")

      if (output$lower_assurance) {
        cat("Assurance might be lower than target assurance at end of study.")
      }
      cat("\n")

      # additional_n
    }
  }

  return(output)

}

## internal functions --------
interim_analysis_sens = function(n11, n22, nT1, nT2, analysis_params) {
  # n11 - test positive, diseased ; n22 - test negative, not disease
  # nT1 - total diseased ; nT2 - total not diseased


  n_T = nT1 + nT2

  additional_n = NA_integer_
  final_width = NA_real_
  lower_assurance = NA
  stop_for_efficacy = FALSE
  stop_for_futility = FALSE

  ap = analysis_params
  alpha = ap$alpha

  # posterior distributions

  sens_design_post = with(ap$sens_design_prior, beta_hyparam(a + n11, b + nT1 - n11))
  sens_analysis_post = with(ap$sens_analysis_prior, beta_hyparam(a + n11, b + nT1 - n11))

  prevalence_post = with(ap$prevalence_design_prior, beta_hyparam(a + nT1, b + n_T - nT1))

  # credible interval
  interim_CI = with(sens_analysis_post, beta_CI(alpha, a, b))


  # check for early stopping for efficacy
  success_at_interim = interim_CI[2] - interim_CI[1] < ap$target_width

  if (success_at_interim) {
    #save final accuracy of interval
    final_width = interim_CI[2] - interim_CI[1]

    # no further participants needed
    additional_n = 0
    stop_for_efficacy = TRUE
  }

  # sample size re-estimation
  else {

    additional_n = min_ss_sensitivity(sens_design_post$a, sens_design_post$b,
                                      sens_analysis_post$a, sens_analysis_post$b,
                                      prevalence_post$a, prevalence_post$b,
                                      ap$target_width, alpha,
                                      1:(ap$max_n - n_T), ap$target_assurance)

    # futility check
    if (is.na(additional_n)) {
      max_n_interim_ass = assurance_sensitivity(ap$max_n - n_T,
                                                sens_design_post$a, sens_design_post$b,
                                                sens_analysis_post$a, sens_analysis_post$b,
                                                prevalence_post$a, prevalence_post$b,
                                                ap$target_width, alpha)

      # stop for futility
      if (max_n_interim_ass < ap$futility_assurance) {
        stop_for_futility = TRUE
      }

      # better than futility, not as good as target
      else{
        ## max_n assurance isn't as high as we want, but we don't stop for futility
        additional_n = ap$max_n - n_T  # just the max able to recruit
        lower_assurance = TRUE ## assurance between futility assurance and target assurance
      }

    }

    else{
      lower_assurance = FALSE
    }


  }


  return(list(additional_n = additional_n,
              credible_interval = interim_CI,
              final_width = final_width,
              lower_assurance = lower_assurance,
              stop_for_efficacy = stop_for_efficacy,
              stop_for_futility = stop_for_futility
  ))

}

interim_analysis_spec = function(n11, n22, nT1, nT2, analysis_params) {
  # n11 - test positive, diseased ; n22 - test negative, not disease
  # nT1 - total diseased ; nT2 - total not diseased

  n_T = nT1 + nT2

  additional_n = NA_integer_
  final_width = NA_real_
  lower_assurance = NA
  stop_for_efficacy = FALSE
  stop_for_futility = FALSE

  ap = analysis_params
  alpha = ap$alpha

  # posterior distributions

  spec_design_post = with(ap$spec_design_prior, beta_hyparam(a + n22, b + nT2 - n22))
  spec_analysis_post = with(ap$spec_analysis_prior, beta_hyparam(a + n22, b + nT2 - n22))

  prevalence_post = with(ap$prevalence_design_prior, beta_hyparam(a + nT1, b + n_T - nT1))

  # credible interval
  interim_CI = with(spec_analysis_post, beta_CI(alpha, a, b))


  # check for early stopping for efficacy
  success_at_interim = interim_CI[2] - interim_CI[1] < ap$target_width

  if (success_at_interim) {
    #save final accuracy of interval
    final_width = interim_CI[2] - interim_CI[1]

    # no further participants needed
    additional_n = 0
    stop_for_efficacy = TRUE
  }

  # sample size re-estimation
  else {

    additional_n = min_ss_specificity(spec_design_post$a, spec_design_post$b,
                                      spec_analysis_post$a, spec_analysis_post$b,
                                      prevalence_post$a, prevalence_post$b,
                                      ap$target_width, alpha,
                                      1:(ap$max_n - n_T), ap$target_assurance)

    # futility check #####
    if (is.na(additional_n)) {
      max_n_interim_ass = assurance_specificity(ap$max_n - n_T,
                                                spec_design_post$a, spec_design_post$b,
                                                spec_analysis_post$a, spec_analysis_post$b,
                                                prevalence_post$a, prevalence_post$b,
                                                ap$target_width, alpha)

      # stop for futility
      if (max_n_interim_ass < ap$futility_assurance) {
        stop_for_futility = TRUE
      }

      # better than futility, not as good as target
      else{
        ## max_n assurance isn't as high as we want, but we don't stop for futility
        additional_n = ap$max_n - n_T  # just the max able to recruit
        lower_assurance = TRUE ## assurance between futility assurance and target assurance
      }

    }

    else{
      lower_assurance = FALSE
    }


  }


  return(list(additional_n = additional_n,
              credible_interval = interim_CI,
              final_width = final_width,
              lower_assurance = lower_assurance,
              stop_for_efficacy = stop_for_efficacy,
              stop_for_futility = stop_for_futility
  ))

}

interim_analysis_joint = function(n11, n22, nT1, nT2, analysis_params) {
  # n11 - test positive, diseased ; n22 - test negative, not disease
  # nT1 - total diseased ; nT2 - total not diseased

  n_T = nT1 + nT2

  additional_n = NA_integer_
  final_width = NA_real_
  lower_assurance = NA
  stop_for_efficacy = FALSE
  stop_for_futility = FALSE

  ap = analysis_params
  alpha = ap$alpha

  # posterior distributions

  sens_design_post = with(ap$sens_design_prior, beta_hyparam(a + n11, b + nT1 - n11))
  sens_analysis_post = with(ap$sens_analysis_prior, beta_hyparam(a + n11, b + nT1 - n11))

  spec_design_post = with(ap$spec_design_prior, beta_hyparam(a + n22, b + nT2 - n22))
  spec_analysis_post = with(ap$spec_analysis_prior, beta_hyparam(a + n22, b + nT2 - n22))

  prevalence_post = with(ap$prevalence_design_prior, beta_hyparam(a + nT1, b + n_T - nT1))

  # credible intervals
  interim_sens_CI = with(sens_analysis_post, beta_CI(alpha, a, b))
  interim_spec_CI = with(spec_analysis_post, beta_CI(alpha, a, b))


  # check for early stopping for efficacy
  success_at_interim = (interim_sens_CI[2] - interim_sens_CI[1] < ap$target_width) &&
    (interim_spec_CI[2] - interim_spec_CI[1] < ap$target_width)

  if (success_at_interim) {
    #save final accuracy of interval
    final_width = list(sens = interim_sens_CI[2] - interim_sens_CI[1],
                       spec = interim_spec_CI[2] - interim_spec_CI[1])

    # no further participants needed
    additional_n = 0
    stop_for_efficacy = TRUE
  }

  # sample size re-estimation
  else {

    additional_n = min_ss_sens_and_spec(sens_design_post$a, sens_design_post$b,
                                        sens_analysis_post$a, sens_analysis_post$b,
                                        spec_design_post$a, spec_design_post$b,
                                        spec_analysis_post$a, spec_analysis_post$b,
                                        prevalence_post$a, prevalence_post$b,
                                        ap$target_width, alpha,
                                        1:(ap$max_n - n_T), ap$target_assurance)

    # futility check
    if (is.na(additional_n)) {
      max_n_interim_ass = assurance_sens_and_spec(ap$max_n - n_T,
                                                  sens_design_post$a, sens_design_post$b,
                                                  sens_analysis_post$a, sens_analysis_post$b,
                                                  spec_design_post$a, spec_design_post$b,
                                                  spec_analysis_post$a, spec_analysis_post$b,
                                                  prevalence_post$a, prevalence_post$b,
                                                  ap$target_width, alpha)

      # stop for futility
      if (max_n_interim_ass < ap$futility_assurance) {
        stop_for_futility = TRUE
      }

      # better than futility, not as good as target
      else{
        ## max_n assurance isn't as high as we want, but we don't stop for futility
        additional_n = ap$max_n - n_T  # just the max able to recruit
        lower_assurance = TRUE ## assurance between futility assurance and target assurance
      }

    }

    else{
      lower_assurance = FALSE
    }


  }


  return(list(additional_n = additional_n,
              credible_interval = list(sens = interim_sens_CI,
                                       spec = interim_spec_CI),
              final_width = final_width,
              lower_assurance = lower_assurance,
              stop_for_efficacy = stop_for_efficacy,
              stop_for_futility = stop_for_futility
  ))

}

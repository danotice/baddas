#' Study parameters
#'
#' Creates a list containing the study parameters. These should be determined before
#' all the data is collected and any analysis begins.
#'
#' @param target_width Numeric, target width for the posterior credible interval on assurance.
#' @param alpha Numeric in (0, 1). Significance level used to define the credible interval.
#' @param min_n,max_n Integer, the minimum/maximum sample size.
#' @param planned_n  Integer, the predetermined initial planned sample size.
#' @param num_interims Non-negative integer, number of interim analyses planned.
#' @param interim_timing Character string, method for timing of interim analyses.
#' @param sens_design_prior,sens_analysis_prior,spec_design_prior,spec_analysis_prior,prevalence_design_prior List
#'    with elements `a` and `b`, the hyperparameters for the Beta prior for
#'    the sensitivity/specificity/prevalence at the design/analysis stage.
#' @param target_assurance Numeric in (0, 1), target assurance for (re)estimating sample sizes.
#' @param futility_assurance Numeric in (0, 1), minimum assurance else study is stopped early for futility.
#'
#' @returns Named list containing defined parameters.
#' @export
#'
#' @examples
#' ## study using sensitivity - only setting relevant parameters
#'
#' analysis_parameters(
#'   target_width = 0.2, alpha = 0.05,
#'   max_n = 800, min_n=100,
#'   target_assurance = 0.8,
#'   num_interims = 2,
#'   sens_design_prior = beta_hyparam(7,3),
#'   sens_analysis_prior = beta_hyparam(1,1),
#'   prevalence_design_prior = beta_hyparam(2,8),
#'   futility_assurance = 0.2
#'   )

analysis_parameters = function(
    target_width = NA_real_, alpha = NA_real_,
    max_n = NA_integer_, min_n = NA_integer_, planned_n = NA_integer_,
    num_interims = NA_integer_, interim_timing = "",
    sens_design_prior = c(), sens_analysis_prior = c(),
    spec_design_prior = c(), spec_analysis_prior = c(),
    prevalence_design_prior = c(), # prevalence_analysis_prior = c(),
    target_assurance = NA_real_, futility_assurance = NA_real_
    ){

  ## TODO - make this an object? not just list
  ## not all need to be set
  ## can set other values directly in list
  params = list(
    target_width = target_width,
    alpha = alpha,

    max_n = max_n,
    min_n = min_n,
    planned_n = planned_n,
    num_interims = num_interims,
    interim_timing = interim_timing,

    sens_design_prior = sens_design_prior,
    sens_analysis_prior = sens_analysis_prior,
    spec_design_prior = spec_design_prior,
    spec_analysis_prior = spec_analysis_prior,
    prevalence_design_prior = prevalence_design_prior,
    # prevalence_analysis_prior = prevalence_analysis_prior,

    target_assurance = target_assurance,
    futility_assurance = futility_assurance
  )

  return(params)
}


#' Summary of study plan
#'
#' Takes all the study parameters, calculates the planned sample size (if not given) and the timing
#' of interim analyses. Summarises (in words) what the parameters mean for the study.
#'
#' @param analysis_params List of all relevant parameters.
#' @param measure Character string specifying the measure of interest.
#'   Options are `"sensitivity"`, `"specificity"`, or abbreviations
#'   (e.g., `"sens"`, `"spec"`), or `"both"`.
#'
#' @returns Named list with components:
#'  \describe{
#'    \item{`$planned_n`}{initial planned sample size}
#'    \item{`$interims_at`}{vector of sample sizes for each interim analysis}
#'    }
#' @export
#'
#' @examples
#' ## study using sensitivity - only setting relevant parameters
#'
#' ap = analysis_parameters(
#'   target_width = 0.2, alpha = 0.05,
#'   max_n = 800, min_n=100,
#'   target_assurance = 0.8,
#'   num_interims = 2,
#'   sens_design_prior = beta_hyparam(7,3),
#'   sens_analysis_prior = beta_hyparam(1,1),
#'   prevalence_design_prior = beta_hyparam(2,8),
#'   futility_assurance = 0.2
#'   )
#'
#' start_of_study_summary(ap, "sensitivity")
#'
start_of_study_summary = function(analysis_params, measure) {
  # takes all model params, calculates the planned sample size, gives timing for interim analysis

  ## TODO - add messages for how interim_n is being calculated
  ## TODO - check that valid priors are entered, set default analysis priors to 1,1?

  ap = analysis_params

  measure = valid_measure(measure)

  if (!is.na(ap$planned_n)) {
    planned_n = ap$planned_n

  }
  else if (measure=="sensitivity"){
    planned_n = with(ap,
      min_sample_size(
        target_assurance, min_n, max_n, measure, target_width, alpha,
        sens_design_prior, sens_analysis_prior,
        prevalence_design_prior)
    )

  }
  else if (measure=="specificity"){
    planned_n = with(ap,
      min_sample_size(
        target_assurance, min_n, max_n, measure, target_width, alpha,
        spec_design_prior, spec_analysis_prior,
        prevalence_design_prior)
    )

  }
  else if (measure=="both"){
    planned_n = min_sample_size_joint(
      ap$target_assurance, ap$min_n, ap$max_n, ap$target_width,ap$alpha,
      ap$sens_design_prior, ap$sens_analysis_prior,
      ap$spec_design_prior, ap$spec_analysis_prior,
      ap$prevalence_design_prior
    )

  }
  else {
    stop("Invalid value for param.
         Please specify which measure(s) you want to base success on:
         sensitivity, specificity or both.")
  }

  ## success criteria
  if (measure=="sensitivity" | measure=="specificity"){

    cat(
      "The study is a success if the ", (1-ap$alpha)*100,
      "% credible interval for ", measure,
      " has a width < ", ap$target_width, ".\n", sep="")

  }
  else if (measure=="both") {

    cat(
      "The study is a success if the joint ", (1-ap$alpha)*100,
      "% credible interval for both sensitivity and specificity has a width < ",
      ap$target_width, ".\n", sep="")

  }



  if (is.na(planned_n)) {
    interim_n = interim_timings(ap$max_n, ap$num_interims, ap$interim_timing)

    cat(
      "None of the sample sizes in the range ", ap$min_n, " - ", ap$max_n,
      " achieve the target assurance of ", ap$target_assurance, ".\n", sep="")
  }
  else {

    interim_n = interim_timings(planned_n, ap$num_interims, ap$interim_timing)

    if (!is.na(ap$planned_n)) {
      cat("The planned sample size is ", planned_n, ".\n", sep="")
    }
    else {
      cat(
        "The minimum sample size necessary to achieve target assurance of ", ap$target_assurance,
        " is ", planned_n, ".\n", sep="")
    }

  }

  cat("No more than ", ap$max_n, " patients should be recruited.\n", sep="")
  cat(
    ap$num_interims, "interim analyses are planned and they should occur when ")
  cat(interim_n$interims_at, sep=", ")
  cat(" total patients have been recruited.\n", sep="")

  return(list(
    planned_n = planned_n,
    interims_at = interim_n$interims_at
  ))
}


#' Final outcome of study
#'
#' Checks whether study has been successful, given the observed data (from a 2x2 contingency table)
#' and the analysis parameters. The study is considered a success if for a given \eqn{\alpha},
#' the width of the central \eqn{100(1-\alpha)}% posterior credible interval for the measure of interest is
#' below the specified target width.
#'
#' @details
#' When `measure="sensitivity"`, `n22` may be set to `NA` without causing an issue. Similarly,
#' when `measure="specificity"`, `n11` may be set to `NA`. Otherwise...
#' @seealso [generate_data_table()] for further details on the input data.
#'
#' @param n11 number of true positives.
#' @param n22 number of true negatives.
#' @param nT1 total number of diseased (true positives + false negatives).
#' @param nT2 total number of non-diseased (false positives + true negatives).
#' @param print logical, if `TRUE` (default), details are printed on screen.
#' @inheritParams start_of_study_summary
#'
#' @returns List with components
#'  \describe{
#'    \item{`$success`}{logical, study outcome success or not.}
#'    \item{`$credible_interval`,`$sens_CI`,`$spec_CI`}{credible interval of relevant measure.}
#'    }
#'
#' @export
#'
#' @examples
#' full_data = generate_data_table(n=412, true_prev=0.2, true_sens=0.9, true_spec=0.8)
#' ## need to complete example
#' # check_study_success(full_data$n11, full_data$n22, full_data$nT1, full_data$nT2,
#' #  ap, "sens"
#' #  )
#' @seealso [interim_analysis()]
check_study_success = function(n11, n22, nT1, nT2, analysis_params, measure, print=TRUE) {
  # n11 - test positive, diseased ; n22 - test negative, not disease
  # nT1 - total diseased ; nT2 - total not diseased

  ap = analysis_params
  alpha = ap$alpha

  measure = valid_measure(measure)

  if (measure=="sensitivity") {
    # posterior distributions
    sens_analysis_post = with(ap$sens_analysis_prior, beta_hyparam(a + n11, b + nT1 - n11))

    # credible interval
    sens_CI = with(sens_analysis_post, beta_CI(alpha, a, b))

    # check for success
    success = sens_CI[2] - sens_CI[1] < ap$target_width

    output = list(success = success,
                  credible_interval = sens_CI,
                  measure = measure)
  }

  else if (startsWith(measure, "spec")) {
    spec_analysis_post = with(ap$spec_analysis_prior, beta_hyparam(a + n22, b + nT2 - n22))

    # credible interval
    spec_CI = with(spec_analysis_post, beta_CI(alpha, a, b))

    # check for success
    success = spec_CI[2] - spec_CI[1] < ap$target_width

    output = list(success = success,
                  credible_interval = spec_CI,
                  measure = measure)
  }

  else if (measure == "both") {
    # posterior distributions
    sens_analysis_post = with(ap$sens_analysis_prior, beta_hyparam(a + n11, b + nT1 - n11))
    spec_analysis_post = with(ap$spec_analysis_prior, beta_hyparam(a + n22, b + nT2 - n22))

    # credible intervals
    interim_sens_CI = with(sens_analysis_post, beta_CI(alpha, a, b))
    interim_spec_CI = with(spec_analysis_post, beta_CI(alpha, a, b))

    # check for success
    success = (sens_CI[2] - sens_CI[1] < ap$target_width) &&
      (spec_CI[2] - spec_CI[1] < ap$target_width)

    output = list(success = success,
                  sens_CI = sens_CI,
                  spec_CI = spec_CI)

  }

  else {
    stop("Invalid value for param.
         Please specify which measure(s) you want to base success on:
         sensitivity, specificity or both.")
  }

  if (print) {
    final_n = nT1 + nT2

    if (output$success) {
      cat("The study was SUCCESSFUL based on the ", final_n, " participants.")
    }
    else {
      cat("The study was NOT successful based on the ", final_n, " participants.")
    }

  }

  return(output)
}

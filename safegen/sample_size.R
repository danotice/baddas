#' Minimum sample size to achieve target assurance
#'
#' Searches the given range to find the minimum sample size where the assurance
#' calculated is greater than/equal to the target assurance.
#'
#' @inheritParams assurance
#' @param target_assurance Numeric in (0, 1).
#' @param min_n,max_n Non-negative integer, limits of range to search
#'
#' @returns Integer value, sample size or `NA` if no value in the given range
#' achieves the target assurance.
#' @export
#'
#' @examples
#' design_prior = beta_hyparam(7,3,"lambda-D")
#' analysis_prior = beta_hyparam(1,1,"lambda-A")
#' prevalence_prior = beta_hyparam(2,8,"rho")
#'
#' min_sample_size(0.8, 100, 1000, "sens", 0.2, 0.05,
#'     design_prior, analysis_prior, prevalence_prior)
#'
#' @details
#' Binary search. Assurance monotonically increases with sample size for sensitivity
#' and specificity individually.
#'
#' @seealso [min_sample_size_joint()]
min_sample_size = function(target_assurance, min_n, max_n, measure, target_width, alpha,
                     design_prior, analysis_prior, prevalence_prior){


  if (startsWith(measure, "sens")){

    if (!startsWith("sensitivity", measure)) message("Calculating minimum sample size based on the sensitivity.")

    # prior hyperparams
    a_lambda_design = design_prior$a
    b_lambda_design = design_prior$b
    a_lambda_analysis = analysis_prior$a
    b_lambda_analysis = analysis_prior$b
    a_rho = prevalence_prior$a
    b_rho = prevalence_prior$b

    min_ss = min_ss_sensitivity(a_lambda_design, b_lambda_design, a_lambda_analysis, b_lambda_analysis,
                              a_rho, b_rho, target_width, alpha, min_n:max_n, target_assurance)

    if (is.na(min_ss)) warning("None of the sample sizes in the given range achieve target assurance.")

    return(min_ss)

  }
  else if (startsWith(measure, "spec")){

    if (!startsWith("specificity", measure)) message("Calculating minimum sample size based on the specificity.")

    # prior hyperparams
    a_theta_design = design_prior$a
    b_theta_design = design_prior$b
    a_theta_analysis = analysis_prior$a
    b_theta_analysis = analysis_prior$b
    a_rho = prevalence_prior$a
    b_rho = prevalence_prior$b


    min_ss = min_ss_specificity(a_theta_design,b_theta_design,a_theta_analysis, b_theta_analysis,
                              a_rho, b_rho, target_width, alpha, min_n:max_n, target_assurance)

    if (is.na(min_ss)) warning("None of the sample sizes in the given range achieve target assurance.")

    return(min_ss)

  }
  else if (measure == "both"){
    stop("Invalid value for measure. Use function `min_sample_size_joint` for both.")

  }
  else {
    stop("Invalid value for measure.
         Please specify which measure(s) you want to calculate assurance for: sensitivity or specificity")

  }
}

#' Minimum sample size to achieve target assurance
#'
#' @inheritParams assurance_joint
#' @inheritParams min_sample_size
#'
#' @returns Integer value, sample size or `NA` if no value in the given range
#' achieves the target assurance.
#' @export
#'
#' @seealso [min_sample_size()]
min_sample_size_joint = function(target_assurance, min_n, max_n, target_width, alpha,
                          sens_design_prior, sens_analysis_prior,
                          spec_design_prior, spec_analysis_prior,
                          prevalence_prior){

  # prior hyperparams
  a_lambda_design = sens_design_prior$a
  b_lambda_design = sens_design_prior$b
  a_lambda_analysis = sens_analysis_prior$a
  b_lambda_analysis = sens_analysis_prior$b

  a_theta_design = spec_design_prior$a
  b_theta_design = spec_design_prior$b
  a_theta_analysis = spec_analysis_prior$a
  b_theta_analysis = spec_analysis_prior$b

  a_rho = prevalence_prior$a
  b_rho = prevalence_prior$b

  min_ss = min_ss_sens_and_spec(a_lambda_design, b_lambda_design, a_lambda_analysis, b_lambda_analysis,
                                 a_theta_design,b_theta_design,a_theta_analysis, b_theta_analysis,
                                 a_rho, b_rho, target_width, alpha, min_n:max_n, target_assurance)

  if (is.na(min_ss)) warning("None of the sample sizes in the given range achieve target assurance.")

  return(min_ss)
}

### internal functions --------
# function to calculate minimum n_T that gives required assurance over sensitivity -
min_ss_sensitivity = function(a_lambda_design,b_lambda_design, a_lambda_analysis, b_lambda_analysis,
                              a_rho, b_rho, target_width, alpha, n_T_range, target_assurance) {
  min_ss = NA_integer_

  low = min(n_T_range)
  high = max(n_T_range)

  assurance_high = assurance_sensitivity(
    high,
    a_lambda_design, b_lambda_design, a_lambda_analysis, b_lambda_analysis,
    a_rho,b_rho,
    target_width, alpha)

  if (assurance_high < target_assurance) {
    # don't bother searching range - return na
  }

  else {

    while (low <= high) {
      mid = floor((low + high) / 2)

      assurance = assurance_sensitivity(
        mid,
        a_lambda_design, b_lambda_design,
        a_lambda_analysis, b_lambda_analysis,
        a_rho, b_rho,
        target_width, alpha)

      if (assurance >= target_assurance) {
        min_ss = mid
        high = mid - 1   # search smaller n
      }
      else {
        low = mid + 1    # search larger n
      }
    }

  }

  return(min_ss)
}


min_ss_specificity = function(a_theta_design,b_theta_design,a_theta_analysis, b_theta_analysis,
                              a_rho, b_rho, target_width, alpha, n_T_range, target_assurance) {
  min_ss = NA_integer_

  low = min(n_T_range)
  high = max(n_T_range)

  assurance_high = assurance_specificity(
    high,
    a_theta_design, b_theta_design, a_theta_analysis, b_theta_analysis,
    a_rho,b_rho,
    target_width, alpha)

  if (assurance_high < target_assurance) {
    # don't bother searching range - return na
  }

  else {

    while (low <= high) {
      mid = floor((low + high) / 2)

      assurance = assurance_specificity(
        mid,
        a_theta_design, b_theta_design, a_theta_analysis, b_theta_analysis,
        a_rho,b_rho,
        target_width, alpha)

      if (assurance >= target_assurance) {
        min_ss = mid
        high = mid - 1   # search smaller n
      }
      else {
        low = mid + 1    # search larger n
      }
    }

  }


  return(min_ss)
}

min_ss_sens_and_spec = function(a_lambda_design, b_lambda_design, a_lambda_analysis, b_lambda_analysis,
                                a_theta_design,b_theta_design,a_theta_analysis, b_theta_analysis,
                                a_rho, b_rho, target_width, alpha, n_T_range, target_assurance) {

  ## this one isn't monotonically increasing, so need to search full range
  assurances = lapply(as.list(n_T_range), assurance_sens_and_spec,
                      a_lambda_design, b_lambda_design, a_lambda_analysis, b_lambda_analysis,
                      a_theta_design, b_theta_design, a_theta_analysis, b_theta_analysis,
                      a_rho, b_rho, target_width, alpha)

  if (any(assurances>target_assurance)) {
    min_ss = n_T_range[which.max(assurances>target_assurance)]
  }
  else{
    min_ss = NA_integer_
  }
  return(min_ss)
}


## original functions - will remove ---------
## slightly modified from original code
min_ss_sensitivity0 = function(a_lambda_design,b_lambda_design, a_lambda_analysis, b_lambda_analysis,
                              a_rho, b_rho, target_width, alpha, n_T_range, target_assurance) {
  min_ss = NA_integer_

  for (ss in n_T_range) {
    assurance = assurance_sensitivity(
      ss,
      a_lambda_design, b_lambda_design, a_lambda_analysis, b_lambda_analysis,
      a_rho,b_rho,
      target_width, alpha)

    if(assurance > target_assurance){
      min_ss = ss
      break
    }
  }

  return(min_ss)
}


min_ss_specificity0 = function(a_theta_design,b_theta_design,a_theta_analysis, b_theta_analysis,
                              a_rho, b_rho, target_width, alpha, n_T_range, target_assurance) {
  min_ss = NA_integer_

  for (ss in n_T_range){
    assurance = assurance_specificity(
      ss,
      a_theta_design, b_theta_design, a_theta_analysis, b_theta_analysis,
      a_rho,b_rho,
      target_width, alpha)

    if(assurance>target_assurance){
      min_ss = ss

      break
    }
  }
  return(min_ss)
}




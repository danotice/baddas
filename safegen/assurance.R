### functions to calculate assurance

#' Assurance over a single measure
#'
#' Assurance calculation based on sensitivity or specificity for a given sample size.
#' The assurance represents the probability of obtaining a central \eqn{100(1-\alpha)}%
#' posterior credible interval at the end of the study which is narrower than a target
#' width, for the chosen measure.
#'
#' @param n_T Non-negative integer, the sample size.
#' @param measure Character string specifying the measure of interest.
#'   Options are `"sensitivity"`, `"specificity"`, or abbreviations
#'   (e.g., `"sens"`, `"spec"`).
#' @param target_width Numeric, target width for the posterior credible interval.
#' @param alpha Numeric in (0, 1). Significance level used to define the
#'   \eqn{100(1 - \alpha)\%} credible interval.
#' @param design_prior List with elements `a` and `b`, the hyperparameters for
#'    the Beta prior at the design stage.
#' @param analysis_prior List with elements `a` and `b`, the hyperparameters for
#'    the Beta prior at the analysis stage. Defaults to Beta(1,1).
#' @param prevalence_prior List with elements `a` and `b`, the hyperparameters for
#'    the Beta prior for prevalence.
#'
#' @returns Numeric value giving the assurance (a probability between 0 and 1).
#' @export
#'
#' @examples
#' design_prior = beta_hyparam(7,3,"lambda-D")
#' analysis_prior = beta_hyparam(1,1,"lambda-A")
#' prevalence_prior = beta_hyparam(2,8,"rho")
#'
#' assurance(750, "sens", 0.2, 0.05, design_prior, analysis_prior, prevalence_prior)
#'
#' @seealso [assurance_joint()]
#'
assurance = function(n_T, measure, target_width, alpha,
                     design_prior, analysis_prior, prevalence_prior){

  if (startsWith(measure, "sens")){

    if (!startsWith("sensitivity", measure))message("Calculating assurance on the sensitivity.")

    # prior hyperparams
    a_lambda_design = design_prior$a
    b_lambda_design = design_prior$b
    a_lambda_analysis = analysis_prior$a
    b_lambda_analysis = analysis_prior$b
    a_rho = prevalence_prior$a
    b_rho = prevalence_prior$b

    return(assurance_sensitivity(n_T,a_lambda_design,b_lambda_design,
                                  a_lambda_analysis, b_lambda_analysis,
                                  a_rho,b_rho,
                                  target_width,alpha)
    )

  }
  else if (startsWith(measure, "spec")){

    if (!startsWith("specificity", measure)) message("Calculating assurance on the specificity.")

    # prior hyperparams
    a_theta_design = design_prior$a
    b_theta_design = design_prior$b
    a_theta_analysis = analysis_prior$a
    b_theta_analysis = analysis_prior$b
    a_rho = prevalence_prior$a
    b_rho = prevalence_prior$b


    return(assurance_specificity(n_T,a_theta_design,b_theta_design,
                                 a_theta_analysis,b_theta_analysis,
                                 a_rho,b_rho,target_width,alpha)
    )
  }
  else if (measure == "both"){
    stop("Invalid value for measure. Use function `assurance_joint` for both.")

  }
  else {
    stop("Invalid value for measure.
         Please specify which measure(s) you want to calculate assurance for: sensitivity or specificity")

  }
}

#' Assurance over sensitivity and specificity jointly
#'
#' @inheritParams assurance
#' @param sens_design_prior List with elements `a` and `b`, the hyperparameters for
#'    the Beta prior for sensitivity at the design stage.
#' @param sens_analysis_prior List with elements `a` and `b`, the hyperparameters for
#'    the Beta prior for sensitivity at the analysis stage. Defaults to Beta(1,1).
#' @param spec_design_prior List with elements `a` and `b`, the hyperparameters for
#'    the Beta prior for specificity at the design stage.
#' @param spec_analysis_prior List with elements `a` and `b`, the hyperparameters for
#'    the Beta prior for specificity at the analysis stage. Defaults to Beta(1,1).
#'
#' @returns Numeric value giving the assurance (a probability between 0 and 1).
#' @export
#'
#' @seealso [assurance()]
#' @details
#' Additional details... Include formula and such maybe.
#'
assurance_joint = function(n_T, target_width, alpha,
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

  return(assurance_sens_and_spec(n_T,a_lambda_design, b_lambda_design,
                                 a_lambda_analysis, b_lambda_analysis,
                                 a_theta_design, b_theta_design,
                                 a_theta_analysis, b_theta_analysis,
                                 a_rho,b_rho,target_width,alpha)
  )


}


#### internal functions ----------
## --- calcs Eq 1 p6 of paper for given nT

#function to calculate assurance over sensitivity for one value of n_T
assurance_sensitivity = function(n_T, a_lambda_design, b_lambda_design,
                                 a_lambda_analysis, b_lambda_analysis,
                                 a_rho, b_rho, target_width, alpha){

  lgamma_rho = lgamma(a_rho + b_rho) - lgamma(a_rho) - lgamma(b_rho)
  to_sum_outer = vector(length = n_T + 1)

  for(nT1 in 0:n_T){
    lambda_L = stats::qbeta(alpha/2, a_lambda_analysis + 0:nT1, b_lambda_analysis + nT1:0)
    lambda_U = stats::qbeta(1-alpha/2, a_lambda_analysis + 0:nT1, b_lambda_analysis + nT1:0)

    target_hit = (lambda_U - lambda_L) <= target_width


    acceptable_n11 = which(target_hit == TRUE) - 1
    if(length(acceptable_n11)>0){
      to_sum_inner = exp(lchoose(nT1,acceptable_n11) + lgamma(a_lambda_design +acceptable_n11)+lgamma(b_lambda_design +  nT1 - acceptable_n11)-
                                                      lgamma(a_lambda_design +b_lambda_design + nT1 ))
      to_sum_outer[nT1 + 1] = sum(to_sum_inner)*exp(lchoose(n_T, nT1) +
        lgamma_rho+lgamma(a_rho + nT1)+lgamma(b_rho+n_T - nT1)-lgamma(a_rho + b_rho + n_T))
    }
    else{
      to_sum_outer[nT1 + 1] = 0
    }


  }
  assurance = exp(lgamma(a_lambda_design + b_lambda_design)-lgamma(a_lambda_design)-lgamma(b_lambda_design))*sum(to_sum_outer)

  return(assurance)
}

assurance_specificity = function(n_T,a_theta_design,b_theta_design, a_theta_analysis,
                                 b_theta_analysis,a_rho,b_rho,target_width,alpha){

  lgamma_rho = lgamma(a_rho + b_rho) - lgamma(a_rho) - lgamma(b_rho)
  to_sum_outer = vector(length = n_T + 1)

  for(nT2 in 0:n_T){

    lambda_L = stats::qbeta(alpha/2, a_theta_analysis + 0:nT2, b_theta_analysis + nT2:0)
    lambda_U = stats::qbeta(1-alpha/2, a_theta_analysis + 0:nT2, b_theta_analysis + nT2:0)

    target_hit = (lambda_U - lambda_L) <= target_width

    acceptable_n22 = which(target_hit == TRUE)-1

    if(length(acceptable_n22)>0){
      to_sum_inner = exp(lchoose(nT2,acceptable_n22) + lgamma(a_theta_design + acceptable_n22) +
                           lgamma(b_theta_design +  nT2 - acceptable_n22)-
                           lgamma(a_theta_design +b_theta_design + nT2 ))
      to_sum_outer[nT2 + 1] = sum(to_sum_inner) * exp(lchoose(n_T, nT2) + lgamma_rho +
              lgamma(a_rho + nT2) + lgamma(b_rho+n_T - nT2) - lgamma(a_rho + b_rho + n_T))
    }
    else{
      to_sum_outer[nT2 + 1] = 0
    }


  }
  assurance = exp(lgamma(a_theta_design + b_theta_design)-lgamma(a_theta_design)-lgamma(b_theta_design))*sum(to_sum_outer)

  return(assurance)
}

assurance_sens_and_spec = function(n_T,a_lambda_design, b_lambda_design,
                                   a_lambda_analysis, b_lambda_analysis,
                                   a_theta_design, b_theta_design,
                                   a_theta_analysis, b_theta_analysis,
                                   a_rho,b_rho,target_width,alpha){

  lgamma_rho = lgamma(a_rho + b_rho) - lgamma(a_rho) - lgamma(b_rho)
  to_sum_outer = vector(length = n_T + 1)

  for(nT1 in 0:n_T){
    nT2 = n_T - nT1

    target_hit_n11=stats::qbeta(1-alpha/2,a_lambda_analysis + 0:nT1 , b_lambda_analysis + nT1 - 0:nT1) -
      stats::qbeta(alpha/2,a_lambda_analysis + 0:nT1, b_lambda_analysis + nT1 - 0:nT1) <=target_width

    acceptable_n11 = which(target_hit_n11 == TRUE)-1

    target_hit_n22=stats::qbeta(1-alpha/2,a_theta_analysis + 0:nT2 , b_theta_analysis + nT2 - 0:nT2) -
      stats::qbeta(alpha/2,a_theta_analysis + 0:nT2, b_theta_analysis + nT2 - 0:nT2) <=target_width

    acceptable_n22 = which(target_hit_n22 == TRUE)-1

    if(length(acceptable_n11)>0){
      to_sum_inner1 = exp(lchoose(nT1,acceptable_n11) + lgamma(a_lambda_design +acceptable_n11)+
                                                       lgamma(b_lambda_design +  nT1 - acceptable_n11)-
                                                       lgamma(a_lambda_design +b_lambda_design + nT1 ))
    }
    else{
      to_sum_inner1 = 0
    }
    if(length(acceptable_n22)>0){


      to_sum_inner2 = exp(lchoose(nT2,acceptable_n22) + lgamma(a_theta_design +acceptable_n22)+
                            lgamma(b_theta_design +  nT2 - acceptable_n22)-
                            lgamma(a_theta_design +b_theta_design + nT2 ))

    }
    else{
      to_sum_inner2 = 0
    }
    to_sum_outer[nT1 + 1] = sum(to_sum_inner1)*sum(to_sum_inner2)*
      exp(lchoose(n_T, nT1)+lgamma_rho + lgamma(a_rho + nT1)+lgamma(b_rho+nT2)-lgamma(a_rho + b_rho + n_T))


  }
  assurance = exp(lgamma(a_lambda_design + b_lambda_design)-lgamma(a_lambda_design)-
                    lgamma(b_lambda_design) + lgamma(a_theta_design + b_theta_design)-
                    lgamma(a_theta_design)-lgamma(b_theta_design))*sum(to_sum_outer)

  return(assurance)
}


## older versions (will remove after tests) ------------
assurance_sensitivityO = function(n_T, a_lambda_design, b_lambda_design,
                                  a_lambda_analysis, b_lambda_analysis,
                                  a_rho, b_rho, target_width, alpha){

  to_sum_outer = vector(length = n_T + 1)

  for(nT1 in 0:n_T){
    target_hit = vector(length = nT1+1)

    for(n11 in 0:nT1){
      lambda_CI = beta_CI(alpha, a_lambda_analysis + n11, b_lambda_analysis + nT1 - n11)
      target_hit[n11+1] = (lambda_CI[2] - lambda_CI[1]) <= target_width
    }

    acceptable_n11 = which(target_hit == TRUE)-1
    if(length(acceptable_n11)>0){
      to_sum_inner = choose(nT1,acceptable_n11)*exp(lgamma(a_lambda_design +acceptable_n11)+lgamma(b_lambda_design +  nT1 - acceptable_n11)-
                                                      lgamma(a_lambda_design +b_lambda_design + nT1 ))
      to_sum_outer[nT1 + 1] = sum(to_sum_inner)*choose(n_T, nT1)*
        exp(lgamma(a_rho + b_rho)-lgamma(a_rho)-lgamma(b_rho)+lgamma(a_rho + nT1)+lgamma(b_rho+n_T - nT1)-lgamma(a_rho + b_rho + n_T))
    }
    else{
      to_sum_outer[nT1 + 1] = 0
    }


  }
  assurance = exp(lgamma(a_lambda_design + b_lambda_design)-lgamma(a_lambda_design)-lgamma(b_lambda_design))*sum(to_sum_outer)

  return(assurance)
}


assurance_specificityO = function(n_T,a_theta_design,b_theta_design, a_theta_analysis,
                                 b_theta_analysis,a_rho,b_rho,target_width,alpha){
  to_sum_outer = vector(length = n_T + 1)
  for(nT2 in 0:n_T){

    target_hit = vector(length = nT2+1)

    for(n22 in 0:nT2){
      theta_CI = beta_CI(alpha, a_theta_analysis + n22, b_theta_analysis + nT2 - n22)
      target_hit[n22+1] = (theta_CI[2] - theta_CI[1]) <= target_width
    }

    acceptable_n22 = which(target_hit == TRUE)-1

    if(length(acceptable_n22)>0){
      to_sum_inner = choose(nT2,acceptable_n22)*exp(lgamma(a_theta_design +acceptable_n22)+lgamma(b_theta_design +  nT2 - acceptable_n22)-
                                                       lgamma(a_theta_design +b_theta_design + nT2 ))
      to_sum_outer[nT2 + 1] = sum(to_sum_inner)*choose(n_T, nT2)*
        exp(lgamma(a_rho + b_rho)-lgamma(a_rho)-lgamma(b_rho)+lgamma(a_rho + nT2)+lgamma(b_rho+n_T - nT2)-lgamma(a_rho + b_rho + n_T))
    }
    else{
      to_sum_outer[nT2 + 1] = 0
    }


  }
  assurance = exp(lgamma(a_theta_design + b_theta_design)-lgamma(a_theta_design)-lgamma(b_theta_design))*sum(to_sum_outer)

  return(assurance)
}

assurance_sens_and_specO = function(n_T,a_lambda_design, b_lambda_design,
                                   a_lambda_analysis, b_lambda_analysis,
                                   a_theta_design, b_theta_design,
                                   a_theta_analysis, b_theta_analysis,
                                   a_rho,b_rho,target_width,alpha){
  to_sum_outer = vector(length = n_T + 1)
  for(nT1 in 0:n_T){
    nT2 = n_T - nT1

    # NOTE - not as simple to change to beta_CI() because of range
    target_hit_n11=stats::qbeta(1-alpha/2,a_lambda_analysis + 0:nT1 , b_lambda_analysis + nT1 - 0:nT1) -
      stats::qbeta(alpha/2,a_lambda_analysis + 0:nT1, b_lambda_analysis + nT1 - 0:nT1) <=target_width

    acceptable_n11 = which(target_hit_n11 == TRUE)-1

    target_hit_n22=stats::qbeta(1-alpha/2,a_theta_analysis + 0:nT2 , b_theta_analysis + nT2 - 0:nT2) -
      stats::qbeta(alpha/2,a_theta_analysis + 0:nT2, b_theta_analysis + nT2 - 0:nT2) <=target_width

    acceptable_n22 = which(target_hit_n22 == TRUE)-1

    if(length(acceptable_n11)>0){
      to_sum_inner1 = choose(nT1,acceptable_n11)*exp(lgamma(a_lambda_design +acceptable_n11)+
                                                        lgamma(b_lambda_design +  nT1 - acceptable_n11)-
                                                        lgamma(a_lambda_design +b_lambda_design + nT1 ))
    }
    else{
      to_sum_inner1 = 0
    }
    if(length(acceptable_n22)>0){


      to_sum_inner2 = choose(nT2,acceptable_n22)*exp(lgamma(a_theta_design +acceptable_n22)+lgamma(b_theta_design +  nT2 - acceptable_n22)-
                                                        lgamma(a_theta_design +b_theta_design + nT2 ))

    }
    else{
      to_sum_inner2 = 0
    }
    to_sum_outer[nT1 + 1] = sum(to_sum_inner1)*sum(to_sum_inner2)*choose(n_T, nT1)*
      exp(lgamma(a_rho + b_rho)-lgamma(a_rho)-lgamma(b_rho)+lgamma(a_rho + nT1)+lgamma(b_rho+nT2)-lgamma(a_rho + b_rho + n_T))


  }
  assurance = exp(lgamma(a_lambda_design + b_lambda_design)-lgamma(a_lambda_design)-
                    lgamma(b_lambda_design) + lgamma(a_theta_design + b_theta_design)-
                    lgamma(a_theta_design)-lgamma(b_theta_design))*sum(to_sum_outer)

  return(assurance)
}


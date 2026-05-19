## check valid inputs

valid_measure = function(param){

  if (startsWith(param, "sens")){

    if (!startsWith("sensitivity", param)) message("Considering the sensitivity parameter.")
    parameter = "sensitivity"
  }

  else if (startsWith(param, "spec")){
    if (!startsWith("specificity", param)) message("Considering the specificity parameter.")
    parameter = "specificity"
  }

  else if (param == "both" || param == "joint"){
    parameter = "both"
  }

  return(parameter)
}

valid_prior = function(prior){

}

valid_data = function(n, n_data){

}

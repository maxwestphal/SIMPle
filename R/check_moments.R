check_moments <- function(moments, nu, input, project, msg=TRUE, ...){
  input.str <- switch(input,
                      corr="specfied correlations",
                      moments="specified moments",
                      gamma="(implicitly) specified moments")
  B <- bound_moments(moments, nu)

  if(project){
    if(any(B$below)){
      if(msg) message("SIMPle: Some ", input.str, " are too low - projecting onto Fréchet bounds!")
    }
    if(any(B$above)){
      if(msg) message("SIMPle: Some ", input.str, " are too high - projecting onto Fréchet bounds!")
    }
    if(!all(B$inside)){
      if(msg) message("SIMPle: ", input.str, " conforming to Fréchet bounds:")
      if(msg) print(B$inside)
    }
    return(B$projected)
  }
  if(!project){
    if(!all(B$inside)){
      if(msg) message("SIMPle: ", input.str, " conforming to Fréchet bounds:")
      if(msg) print(B$inside)
      stop("Fréchet bounds (partially) violated!")
    }
    return(B$projected)
  }
}

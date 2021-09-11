check_mean <- function(vars, mean, ...){
  if(is.null(mean)){mean <- 1/2}
  stopifnot(is.numeric(mean))
  stopifnot(length(mean) %in% c(1, vars))
  if(length(mean) == 1){mean <- rep(mean, vars)}
  return(mean)
}

#' @importFrom matrixcalc is.positive.definite
check_corr <- function(vars, corr, ...){
  if(is.null(corr)){corr <- 0}
  if(is.null(dim(corr)) & is.numeric(corr)){
    corr <- rho2corr(vars, corr)
  }
  stopifnot(nrow(corr) == vars & ncol(corr) == vars)
  stopifnot(matrixcalc::is.positive.definite(corr))
  return(corr)
}

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

check_adm <- function(lower, upper, central=NULL){
  stopifnot(length(lower) == length(upper))
  if(!is.null(central)){
    stopifnot(length(lower) == length(central))
    return(all(lower <= central) & all(upper >= central))
  }
  return(all(lower <= upper))
}

#' @importFrom matrixcalc is.positive.definite
rho2corr <- function(vars, rho){
  if(vars==1){
    return(matrix(1))
  }
  if(is.matrix(rho)){
    stopifnot(matrixcalc::is.positive.definite(rho))
    return(rho)
  }
  matrix(rho, vars, vars) + diag(rep(1-rho, vars))
}

corr2moments <- function(vars, nu, mean, corr, ...){
  args <- c(as.list(environment()), list(...))
  mean <- do.call(check_mean, args)
  corr <- do.call(check_corr, args)
  if(nu==0){return(matrix(0, vars, vars))}
  alpha <- mean*nu; beta <- nu-alpha
  sigma <- cor2cov(corr, sqrt(alpha*beta/(nu^2*(nu+1))))
  return(sigma*nu*(1+nu) + (alpha %*% t(alpha))/nu )
}

gamma2moments <- function(gamma, ...){
  nu <- sum(gamma)
  vars <- log(length(gamma), 2); stopifnot(vars > 0 & vars %% 1 == 0)
  H <- Hmat(vars)
  return(H %*% diag(gamma) %*% t(H))
}

corr2moments <- function(vars, nu, mean, corr, ...){
  args <- c(as.list(environment()), list(...))
  mean <- do.call(check_mean, args)
  corr <- do.call(check_corr, args)
  if(nu==0){return(matrix(0, vars, vars))}
  alpha <- mean*nu; beta <- nu-alpha
  sigma <- cor2cov(corr, sqrt(alpha*beta/(nu^2*(nu+1))))
  return(sigma*nu*(1+nu) + (alpha %*% t(alpha))/nu )
}

cov_mbeta <- function(vars=NULL, nu=NULL, mean=NULL, corr=NULL, moments=NULL, gamma=NULL, ...){
  index <- which.max(c(!is.null(vars) & !is.null(nu) & !is.null(mean) & !is.null(corr),
                       !is.null(moments) & !is.null(nu),
                       !is.null(gamma)))
  stopifnot(any(index))
  input <- c("corr", "moments", "gamma")[index]

  moments <- switch(input,
                    corr = corr2moments(vars, nu, mean, corr),
                    moments = moments,
                    gamma = gamma2moments(gamma))

  nu <- switch(input, corr = nu, moments = nu, gamma = sum(gamma))

  vars <- nrow(moments)
  alpha <- diag(moments)
  return( (moments - (alpha %*% t(alpha))/nu) / (nu*(1+nu)) )
}



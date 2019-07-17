### Beta DISTRIBUTION:
get_mode_Beta <- function(...){
  return(NULL)
}

params_Beta <- function(alpha, beta, ...){
  stopifnot(alpha >= 0 & beta >= 0)
  return(list(alpha=alpha, beta=beta))
}

features_Beta <- function(alpha, beta, ...){
  list(support = 0:1,
       mean = mean_Beta(alpha, beta),
       mode = mode_Beta(alpha, beta),
       var  =  var_Beta(alpha, beta))
}

mean_Beta <- function(alpha, beta){
  alpha / (alpha+beta)
}

mode_Beta <- function(alpha, beta){
  if(alpha >  1 & beta >  1){return((alpha-1)/(alpha+beta-2))}
  if(alpha == 1 & beta >  1){return(0)}
  if(alpha >  1 & beta == 1){return(1)}
  return(NA)
}

var_Beta <- function(alpha, beta){
  (alpha*beta)/((alpha+beta)^2*(alpha+beta+1))
}

check_data_Beta <- function(dist, data, ...){
  stopifnot(inherits(data, c("integer", "matrix")))
  stopifnot(all(data %in% c(0,1)))
  if(is.matrix(data)){
    stopifnot(ncol(data)==1)
  }
  return(invisible())
}

update_Beta <- function(dist, data, control=NULL, ...){
  stopifnot(any(ncol(data)==1, is.numeric(data)))
  p <- dist$params
  define_Beta(alpha=p$alpha + sum(data==1), beta=p$beta + sum(data==0))
}

### mBeta DISTRIBUTION
get_mode_mBeta <- function(dims, mode=c("auto", "full", "reduced"), ...){
  mode <- match.arg(mode)
  if(mode=="reduced" & dims==2){
    message("SIMPle: Using full parametrization for bivariate case.")
    mode <- "full"
  }
  if(mode=="full" & dims>10){
    warning("SIMPle: Full mBeta parametrization not recommended for dimensions > 10!")
  }
  if(mode=="auto"){
    mode <- ifelse(dims>10, "reduced", "full")
  }
  return(mode)
}

params_mBeta <- function(dims=2, nu=1, mean=1/2, corr=0,
                         moments=NULL, gamma=NULL,
                         vars=NULL, mode=c("auto", "full", "reduced"), tol=0.01,
                         project=TRUE, input, ...){
  args <- c(as.list(environment()), list(...))

  # args$corr <- rho2corr(dims, corr)
  # args$moments <- switch(input,
  #                        corr = do.call(corr2moments, args),
  #                        moments = moments,
  #                        gamma = unname(do.call(gamma2moments, args)))
  #
  # args$moments <- do.call(check_moments, args)

  args$gamma <- switch(mode,
                       reduced = NA,
                       full = switch(input,
                                     corr = do.call(construct_gamma, args),
                                     moments = do.call(construct_gamma, args),
                                     gamma = unname(gamma)))

  list(nu = nu,
       moments = args$moments,
       gamma   = args$gamma)
}

features_mBeta <- function(dims=2, nu=1, mean=1/2, corr=0,
                           moments=NULL, gamma=NULL,
                           vars=NULL, mode=c("auto", "full", "reduced"),
                           tol=0.01, ...){
  args <- c(as.list(environment()), list(...))
  list(support = 0:1,
       margins = do.call(margins_mBeta, args),
       cov = cov_mBeta(dims=dims, nu=nu, moments=moments))
}

margins_mBeta <- function(moments, nu, ...){
  return(lapply(diag(moments), function(a) define_Beta(a, nu-a)))
}

cov_mBeta <- function(dims=NULL, nu=NULL, mean=NULL, corr=NULL, moments=NULL, gamma=NULL, ...){
  index <- which.max(c(!is.null(dims) & !is.null(nu) & !is.null(mean) & !is.null(corr),
                       !is.null(moments) & !is.null(nu),
                       !is.null(gamma)))
  stopifnot(any(index))
  input <- c("corr", "moments", "gamma")[index]

  moments <- switch(input,
                    corr = corr2moments(dims, nu, mean, corr),
                    moments = moments,
                    gamma = gamma2moments(gamma))

  nu <- switch(input, corr = nu, moments = nu, gamma = sum(gamma))

  dims <- nrow(moments)
  alpha <- diag(moments)
  return( (moments - (alpha %*% t(alpha))/nu) / (nu*(1+nu)) )
}

gamma2moments <- function(gamma, ...){
  nu <- sum(gamma)
  dims <- log(length(gamma), 2); stopifnot(dims > 0 & dims %% 1 == 0)
  #G <- matrix(gamma, nrow=length(gamma), ncol=dims, byrow = FALSE)
  H <- Hmat(dims)
  #return(H %*% (t(H)*G))
  return(H %*% diag(gamma) %*% t(H))
}

corr2moments <- function(dims, nu, mean, corr, ...){
  args <- c(as.list(environment()), list(...))
  mean <- do.call(check_mean, args)
  corr <- do.call(check_corr, args)
  if(nu==0){return(matrix(0, dims, dims))}
  alpha <- mean*nu; beta <- nu-alpha
  sigma <- cor2cov(corr, sqrt(alpha*beta/(nu^2*(nu+1))))
  return(sigma*nu*(1+nu) + (alpha %*% t(alpha))/nu )
}

mBeta2uBeta <- function(nu, moments, mean, corr, gamma, input, vars, ...){
  message("SIMPle: Returning univariate Beta distribution instead of mBeta with dim=1.")
  switch(input,
         gamma = define_Beta(alpha=gamma[2], beta=gamma[1], vars=vars),
         moments = define_Beta(alpha=as.numeric(moments), beta=nu-as.numeric(moments), vars=vars),
         corr = define_Beta(alpha=mean*nu, beta=(1-mean)*nu, vars=vars))
}

check_data_mBeta <- function(dist, data, ...){
  stopifnot(is.matrix(data))
  stopifnot(all(data %in% c(0,1)))
  stopifnot(ncol(data)==dims(dist), ...)
  return(invisible())
}

update_mBeta <- function(dist, data, control=NULL, ...){
  if(dist$type[2] == "reduced"){
    umoments <- dist$params$moments + gamma2moments(update_gamma(data=data, prior=0, control=control))
    udist <- define_mBeta(dims = dims(dist), nu=dist$params$nu + nrow(data),
                          moments=umoments, mode="reduced")
  }else{
    udist <- define_mBeta(dims=dims(dist),
                          gamma=update_gamma(data=data, prior=dist$params$gamma, control=control),
                          mode = "full")
  }
  udist$transform = dist$transform
  return(udist)
}

#' @importFrom  MCMCpack rdirichlet
#' @importFrom  copula normalCopula
#' @importFrom  copula P2p
#' @importFrom  copula mvdc
#' @importFrom  copula rMvdc
sample_mBeta <- function(dist, method = c("auto", "full", "reduced"), n=10000, proj.pd = TRUE, msg=TRUE){
  method <- match.arg(method)
  if(method=="full" & dist$type[2] == "reduced"){
    message("SIMPle: Dirichlet sampling not possible for reduced mBeta distribution - switching to Copula mode!")
  }
  if(method=="auto"){method <- dist$type[2] #ifelse(dist$type[2] == "full", "dirichlet", "copula")
  }
  if(msg)message(c("SIMPle: Drawing sample from ", dist$type[1], " distribution (method: ", method, ")"))
  if(method=="full"){
    return(Dir2mBeta(MCMCpack::rdirichlet(n=n, alpha=dist$params$gamma)))
  }
  if(method=="reduced"){
    R <- cov2cor(dist$features$cov)
    if(proj.pd){R <- as.matrix(Matrix::nearPD(R)$mat)}
    cop <- normalCopula(param=P2p(R), dim=dims(dist), dispstr = "un")
    mp <- lapply(margin_params(dist), function(x) {names(x) <- c("shape1", "shape2"); x} )
    mvd <- mvdc(cop, margins = rep("beta", dims(dist)), paramMargins = mp)
    return(rMvdc(mvd, n=n))
  }
}

#' @importFrom psych pairs.panels
visualize_mBeta <- function(dist, S=NULL, ...){
  # GRID with marginal density, CR, off-diagonal: simulated data; corr coef
  # TODO: adjust alpha argument for multivariate credible intervals (output message)
  # TODO: USE BETA DENSITY, NOT GAUSSIAN
  # TODO: SET RANGE FROM 0 TO 1 => dist$supp
  if(is.null(S)){S <- dist %>% sample(...)}
  pairs_dist(dist=dist, S, ...)
}

# GLOBAL OBJECT DIST:
DIST <- list()
DIST[["Beta"]] <- list(mode = get_mode_Beta,
                       dim.range = c(1, 1),
                       support = c(0, 1),
                       params = params_Beta,
                       features = features_Beta,
                       check_data = check_data_Beta,
                       update = update_Beta,
                       sample = NA,
                       visualize = NA,
                       ddist = dbeta,
                       pdist = pbeta,
                       qdist = qbeta,
                       rdist = rbeta)

DIST[["mBeta"]] <- list(mode = get_mode_mBeta,
                        dim.range = c(1, Inf),
                        support = c(0, 1),
                        params = params_mBeta,
                        features = features_mBeta,
                        check_data = check_data_mBeta,
                        update = update_mBeta,
                        sample = sample_mBeta,
                        visualize = visualize_mBeta,
                        ddist = dbeta,
                        pdist = pbeta,
                        qdist = qbeta,
                        rdist = rbeta)

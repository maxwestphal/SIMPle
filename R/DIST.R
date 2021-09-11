# mbeta distribution --------------------------------------------------------------------------
params_mbeta <- function(vars=2, nu=1, mean=1/2, corr=0,
                         moments=NULL, gamma=NULL,
                         mode=c("auto", "full", "reduced"),
                         tol=0.01,
                         project=TRUE, input, ...){
  args <- c(as.list(environment()), list(...))

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

features_mbeta <- function(vars=2, nu=1, mean=1/2, corr=0,
                           moments=NULL, gamma=NULL,
                           mode=c("auto", "full", "reduced"),
                           tol=0.01, ...){
  args <- c(as.list(environment()), list(...))
  list(mean = diag(moments)/nu,
       cov = cov_mbeta(vars=vars, nu=nu, moments=moments))
}

check_data_mbeta1 <- function(dist, dat, ...){
  stopifnot(is.matrix(dat))
  stopifnot(all(dat %in% c(0,1)))
  stopifnot(ncol(dat)==vars(dist))
  return(TRUE)
}

check_data_mbeta <- function(dist, data, ...){
  stopifnot(is.list(dist))
  stopifnot(is.list(data))
  stopifnot(length(dist) == length(data))
  stopifnot(all(sapply(1:length(dist), function(g) check_data_mbeta1(dist, data[[g]]))))
  return(TRUE)
}

update_mbeta <- function(dist, data, control=NULL, ...){
  G <- length(dist); udist <- dist
  for(g in 1:G){
    if(type(dist)[2] == "reduced"){
      umoments <- params(dist, g)$moments + (t(data[[g]]) %*% data[[g]])
      udist[[g]] <- define_dist(vars = vars(dist),
                                nu=params(dist, g)$nu + nrow(data[[g]]),
                                moments=umoments, mode="reduced", msg=FALSE)[[1]]
    }else{
      udist[[g]] <- define_dist(gamma = update_gamma(data=data[[g]],
                                                     prior=params(dist, g)$gamma,
                                                     control=control),
                                mode = "full", msg=FALSE)[[1]]
    }
  }
  return(udist)
}

sample_mbeta <- function(dist, n=10000,
                         method = c("auto", "full", "reduced"),
                         proj.pd = TRUE, msg=TRUE){
  method <- match.arg(method)
  if(method=="full" & type(dist)[2] == "reduced"){
    stop("SIMPle: Dirichlet sampling not feasible for reduced mbeta distribution!")
  }
  if(method=="auto"){method <- type(dist)[2]}

  result <- lapply(1:length(dist), function(g){
    sample_mbeta1(dist, g=1, n, method, proj.pd, msg)
  })
  return(result)
}

#' @importFrom  MCMCpack rdirichlet
#' @importFrom  copula normalCopula
#' @importFrom  copula P2p
#' @importFrom  copula mvdc
#' @importFrom  copula rMvdc
sample_mbeta1 <- function(dist, g=1, n=10000, method = "reduced", proj.pd = TRUE, msg=TRUE){
  if(msg)message(c("SIMPle: Drawing sample from ", type(dist)[1], " distribution (method: ", method, ")"))
  if(method=="full"){
    return(dir2mbeta(MCMCpack::rdirichlet(n=n, alpha=params(dist, g, simplify=TRUE)$gamma)))
  }
  if(method=="reduced"){
    if(!proj.pd){R <- cov2cor(features(dist, g)$cov)}
    if(proj.pd){R <- cov2cor(as.matrix(Matrix::nearPD(features(dist, g)$cov)$mat))}
    cop <- copula::normalCopula(param=copula::P2p(R), dim=vars(dist), dispstr = "un")
    mp <- margin_params(dist, g) %>% lapply(as.list) %>% lapply(setNames, nm=c("shape1", "shape2"))
    mvd <- copula::mvdc(cop, margins = rep("beta", vars(dist)), paramMargins = mp)
    return(copula::rMvdc(mvd, n=n))
  }
}

visualize_mbeta <- function(dist, S=NULL, ...){
  ## TODO: groups > 1
  if(is.null(S)){S <- dist %>% draw_sample(...)}
  pairs_dist(dist=dist, S, ...)
}

margin_params_mbeta <- function(dist, which=1:length(dist), simplify=TRUE){
  out <- lapply(which, function(g){
    alpha <- diag(params(dist, g)$moments)
    nu <- params(dist, g)$nu
    lapply(1:vars(dist), function(j) c(alpha=alpha[j], beta=nu-alpha[j]))
  })
  if(length(out) == 1 & simplify){
    return(out[[1]])
  }
  return(out)
}

# Global DIST object --------------------------------------------------------------------------
DIST <- list()

DIST[["mbeta"]] <- list(dim.range = c(1, Inf),
                        support = c(0, 1),
                        params = params_mbeta,
                        features = features_mbeta,
                        check_data = check_data_mbeta,
                        update = update_mbeta,
                        sample = sample_mbeta,
                        visualize = visualize_mbeta,
                        margin_params = margin_params_mbeta,
                        ddist = dbeta,
                        pdist = pbeta,
                        qdist = qbeta,
                        rdist = rbeta)


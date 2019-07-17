#' Draw a sample from a (multivariate) probability distribution
#'
#' @param dist SIMPle.Dist or SIMPle.mDist, specifies distribution to sample from
#' @param method character; specifies sampling method. Either "mcmc" for Monte Carlo (MC)
#' Simulation or "copula" for univariate MC + Copula approach
#' @param n integer; number of sampled observations
#'
#' @return A data matrix of size Nrep x dist$dim
#' @export
#'
#' @examples +++ TODO +++
# sample <- function(dist, method=c("auto", "dirichlet", "copula"), n=10000, seed=1){
#   UseMethod("sample", dist)
# }
sample <- function(dist, method="auto", n="auto", seed=1, msg=TRUE, ...){
  dlist <- dist.in(dist)
  if(n=="auto"){
    n <- round(min(10000, 50000/sqrt(dims(dlist)[2])))
  }
  result <- lapply(1:length(dlist), function(i) {sample1(dlist[[i]], method, n, NULL, msg, ...)}) ################################## TODO: SAME SEED FOR BOTH?!??!?!
  return(list.out(result))
}

sample1 <- function(dist, method="auto", n=10000, seed=1, msg=TRUE, ...){
  #set.seed(seed)
  D <- DIST[[dist$type[1]]]
  do.call(what = D$sample,
          args = list(dist=dist, method=method, n=n, msg=msg))
}

dist.in <- function(dist){
  if(is.SIMPle.dlist(dist)){return(dist)}
  if(is.SIMPle.dist(dist)){return(define_dlist(dist))}
  stop("dist needs to be of class SIMPle.dist or SIMPle.dlist")
}

dist.out <- function(dlist){
  stopifnot(is.SIMPle.dlist(dlist))
  if(length(dlist) == 1){return(dlist[[1]])}
  return(dlist)
}

list.out <- function(l){
  stopifnot(is.list(l))
  if(length(l) == 1){return(l[[1]])}
  return(l)
}

list.in <- function(l){
  if(is.list(l)){return(l)}
  return(list(l))
}






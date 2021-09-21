#' Draw a sample from a (multivariate) probability distribution
#'
#' @param dist SIMPle.dist or SIMPle.dlist, specifies distribution to sample from
#' @param method character, specifies sampling method. Either "mcmc" for Monte Carlo (MC)
#' Simulation or "copula" for univariate MC + Copula approach
#' @param n integer, number of sampled observations
#' @param msg logical, should messages be displayed?
#' @param ... further args (currently unused)
#'
#' @return A data matrix representing the sample
#' @export
draw_sample <- function(dist, n=10000, method="auto", msg=TRUE, ...){
  UseMethod("draw_sample", dist)
}

#' @export
draw_sample.SIMPle.dist <- function(dist, n=10000, method="auto", msg=TRUE, ...){
  D <- DIST[[type(dist)[1]]]
  result <- do.call(what = D$sample,
                    args = list(dist=dist, n=n,
                                method=method,
                                proj.pd = TRUE, msg=TRUE))
  attributes(result) <- attributes(dist)
  class(result) <- c("list", "SIMPle.sample")
  attr(result, "type") <- c(attr(result, "type")[1], "raw")
  return(result)
}








#' Visualize a probability distribution.
#'
#' @param dist \code{SIMPle.dist}, a distribution to visualize.
#' @param details integer, specfies what should be plotted. Possible values are 0 (marginal and
#' bivariate densities, default), 1 (quantiles CDF), 2 (extremes histogram), 3 (exceedence frequency)
#' @param ...
#'
#' @return NONE
#' @export
#'
#' @examples +++ TODO +++
visualize <- function(dist, details=0, ...){
  args <- c(as.list(environment()), list(...))
  args$S <- sample(dist, ...); args$details <- NULL
  stopifnot(is.SIMPle.dist(dist))
  if(details == 1){
    do.call(quantiles_cdf, args)
  }
  if(details == 2){
    do.call(extremes_hist, args)
  }
  if(details == 3){
    do.call(excount_dist, args)
  }
  if(!details){
    do.call(DIST[[dist$type[1]]]$visualize, args)
  }
  return(invisible(dist))
}


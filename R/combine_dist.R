#' Combine subgroup populations into single \code{SIMPle.dist} object
#'
#' @param ... multiple (named) \code{SIMPle.dist} objects with same number of variables
#' @param dists (named) list of further \code{SIMPle.dist} objects
#'
#' @return A \code{SIMPle.dist} object
#' @export
combine_groups <- function(..., dists=NULL){
  dists <- c(list(...), dists) %>% unname()
  stopifnot(length(dists) > 0)
  stopifnot(all(sapply(dists, inherits, what="SIMPle.dist")))
  stopifnot(all(diff(sapply(dists, vars))==0))
  dist <- do.call(c, dists)
  gn <- names(dist)
  mostattributes(dist) <- attributes(dists[[1]])
  attr(dist, "groups") <- length(dist)
  attr(dist, "groupnames") <- gn
  return(dist)
}

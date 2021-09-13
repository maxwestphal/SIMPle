#' Select (sub)groups of a distribution
#'
#' @param dist \code{SIMPle.dist} object
#' @param selection integer vector, describes new group order/selection
#'
#' @return A \code{SIMPle.dist} object, potentially with less groups
#' @export
select_groups <- function(dist, selection){
  stopifnot(inherits(dist, "SIMPle.dist"))
  stopifnot(selection %in% 1:groups(dist))
  udist <- dist[selection] %>% copy_attr(dist, what=NULL)
  attr(udist, "groups") <- length(selection)
  attr(udist, "groupnames") <- attr(dist, "groupnames")[selection]
  return(udist)
}


#' Update a prior distribution with observed data
#'
#' @param dist SIMPle.Dist object,
#' @param data data matrix of suitable type
#' @param control character, specifies adjustment method
#'
#' @return A SIMPle.Dist object, specifying the posterior distribution
#' @export
update_dist <- function(dist, data, control=list()){
  UseMethod("update_dist", dist)
}

#' @export
update_dist.SIMPle.dist <- function(dist, data, control=list()){
  args <- as.list(environment())
  if(groups(dist) == 1 & is.matrix(data)){data <- list(data)}
  D <- DIST[[type(dist)[1]]]
  D$check_data(dist, data)
  do.call(D$update, args = args)
}

#' Split up data matrix by subgroups
#'
#' @param data matrix, contains (binary 0/1) data
#' @param by integer vector
#' @param names character, names of subgroups
#' @param warn integer, a warning is given if \code{length(unique(by)) > warn}
#'
#' @return a list of data matrices
#' @export
#'
#' @details useful to pass data to \code{\link{update_dist}} if \code{groups(dist) > 1}
split_data <- function(data, by=rep(1, nrow(data)), names=unique(by), warn=10){
  stopifnot(all.equal(nrow(data), length(by), length(names)))
  u <- unique(by); stopifnot(length(names) == length(u))
  if(length(u) > warn){warning("Data split into ", length(u), " subgroups. Intended?")}
  dl <- lapply(u, function(x){data[by==u, ]})
  names(dl) <- names
  return(dl)
}

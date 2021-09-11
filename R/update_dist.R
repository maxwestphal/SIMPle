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


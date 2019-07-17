#' Update a prior distribution through data
#'
#' @param dist SIMPle.Dist object,
#' @param data data matrix of suitable type
#' @param control character, specifies adjustment method
#'
#' @return A SIMPle.Dist object, specifying the posterior distribution
#' @export
#'
#' @examples +++ TODO +++
update <- function(dist, data, control=NULL){
 UseMethod("update", dist)
}

#' @export
update.SIMPle.dist <- function(dist, data, control=NULL){
  args <- as.list(environment())
  D <- DIST[[dist$type[1]]]
  D$check_data(dist, data)
  do.call(what = D$update, args = args)
}

#' @export
update.SIMPle.dlist <- function(dist, data, control=NULL){
  stopifnot(is.list(data) & length(dist) == length(data))
  define_dlist(dlist=lapply(1:length(dist), function(d) update(dist[[d]], data[[d]], control)))
}









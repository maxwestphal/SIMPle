#' Define a list ob SIMPle.dist objects.
#'
#' Specify different distributions, e.g. related to the same varibale(s)
#' on different subsets of the data.
#'
#' @param ... \code{SIMPle.dist} objects
#' @param dlist \code{list}, a list of \code{SIMPle.dist} objects
#'
#' @return A \code{SIMPle.dlist}, a list ob \code{SIMPle.dist} obejcts.
#' @export
#'
#' @examples
#' # Define priors for sensitivity (se) and specificity (sp) of a binary classifier:
#' define_dlist(se = define_Beta(4,1), sp = define_Beta(3,2))
define_dlist <- function(..., dlist=NULL){
  dlist <- append(list(...), dlist)
  check_dlist(dlist)
  class(dlist) <- append(class(dlist), "SIMPle.dlist")
  attr(dlist, "vars") <- vars(dlist[[1]])
  attr(dlist, "dims") <- c(length(dlist), dims(dlist[[1]]))
  return(dlist)
}

check_dlist <- function(dlist){
  stopifnot(all(sapply(dlist, is.SIMPle.dist)))
  if(length(dlist)==1){return(TRUE)}
  stopifnot(do.call(all.equal, unname(lapply(dlist, dims))))
  stopifnot(do.call(all.equal, unname(lapply(dlist, vars))))
}

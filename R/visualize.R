#' Visualize a distribution or sample
#'
#' @param x a SIMPle.dist or SIMPle.sample object
#' @param group integer, selects group to be plotted
#' @param inference SIMPle.inference object, or list of arguments passed to \code{\link{infer}}.
#' @param subset integer, set number of points to draw. NULL (default), draws all points.
#' @param ... named arguments passed to \code{\link{draw_sample}} and \code{\link{GGally::ggpairs}}
#' @return
#' @export
visualize <- function(x, group=1, inference=NULL, subset = NULL,...){
  UseMethod("visualize", x)
}

# TODO: multiple groups input
# TODO: check plotting options work: mapping=ggplot2::aes(alpha=0.5)
# TODO: allow inference input
# TODO: for dist inference: take corr paramters from dist object

#' @export
visualise <- visualize

#' @importFrom GGally ggpairs
#' @export
visualize.SIMPle.sample <- function(x, group=1, inference=NULL, subset = NULL,...){
  stopifnot(group %in% 1:length(x))
  subset_sample1(x[[group]], subset) %>%
    as.data.frame() %>%
    GGally::ggpairs()
}

#' @export
visualize.SIMPle.dist <- function(x, group=1, inference=NULL, subset = NULL,...){
  visualize(draw_sample(x, ...), ...)
}

subset_sample1 <- function(sample, subset=NULL){
  if( any(!is.finite(subset)) | any(is.null(subset)) ){
    return(sample)
  }
  stopifnot(is.numeric(sample))
  stopifnot(all(subset %in% 1:nrow(sample)))
  if(length(subset) == 1){
    subset <- base::sample(nrow(sample), subset, replace=FALSE)
  }
  return(sample[subset, ])
}

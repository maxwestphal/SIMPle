#' Visualize a distribution or sample
#'
#' @param x a  \code{SIMPle.dist} or \code{SIMPle.sample} object
#' @param group integer, selects group to be plotted
#' @param inference \code{SIMPle.result} object (currently ignored!)
#' @param subset integer, set number of points to draw. NULL (default), draws all points.
#' @param ... named arguments passed to \code{\link{draw_sample}} and \code{GGally::ggpairs},
#' e.g. set alpha via \code{mapping=ggplot2::aes(alpha=0.5)}
#' @return
#' @export
visualize <- function(x, group=1, inference=NULL, subset = NULL,...){
  UseMethod("visualize", x)
}

# TODO: utilize inference input

#' @export
visualise <- visualize

#' @importFrom GGally ggpairs
#' @export
visualize.SIMPle.sample <- function(x, group=NULL, inference=NULL, subset = NULL,...){
  if(is.null(group)){group <- 1:length(x)}
  stopifnot(group %in% 1:length(x))
  data <- sapply(group, function(g){subset_sample1(x[[g]], subset=subset)}) %>% as.data.frame()
  colnames(data) <- paste0(rep(names(x), each=ncol(x[[1]])), "_",
                           rep(colnames(x[[1]]), length(x)))
  GGally::ggpairs(as.data.frame(data))
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

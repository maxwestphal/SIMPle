#' Visualize a distribution or sample
#'
#' @param x a  \code{SIMPle.dist} or \code{SIMPle.sample} object
#' @param group integer, selects group to be plotted
#' @param inference \code{SIMPle.result} object (currently ignored!)
#' @param subset integer, set number of points to draw. NULL (default), draws all points.
#' @param ... named arguments passed to \code{\link{draw_sample}} and \code{GGally::ggpairs},
#' e.g. set alpha via \code{mapping=ggplot2::aes(alpha=0.5)}
#' @return GGally::ggpairs plot applied to (prior/posterior) sample
#' @export
visualize <- function(x, group=1, inference=NULL, subset = NULL,...){
  UseMethod("visualize", x)
}

#' @importFrom GGally ggpairs
#' @export
visualize.SIMPle.sample <- function(x, group=1, inference=NULL, subset = NULL,...){
  stopifnot(group %in% 1:length(x))
  data <- do.call(dplyr::bind_cols,
                  lapply(group, function(g){as.data.frame(subset_sample1(x[[g]], subset=subset))} ))
  colnames(data) <- get_labels(x=x, group=group)
  GGally::ggpairs(data)
}

get_labels <- function(x, group){
  if(length(group) == 1){
    gn <- ""
  }
  if(length(group) > 1 & is.null(groupnames(x))){
    gn <- paste0("group", 1:length(x), "_")
  }
  if(length(group) > 1 & !is.null(groupnames(x))){
    gn <- paste0("group", 1:length(x), "_")
  }
  v <- ifelse(is.null(vars(x)), ncol(x[[1]]), vars(x))
  if(is.null(varnames(x))){
    vn <- paste0("var", 1:v)
  }
  if(!is.null(varnames(x))){
    vn <- varnames(x)
  }
  labels <- paste0(rep(gn, each=v), rep(vn, length(x)))
  return(labels)
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

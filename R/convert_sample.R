#' Apply functions to SIMPle.sample objects
#'
#' @param sample \code{SIMPle.sample} object, see \code{\link{draw_sample}}
#' @param margin either integer vector with values 1,2 or 3 or character vector with values
#' "obs", "var" or "group" of maximum length 3. Function fun is applied in the specified order.
#' @param fun function to be applied
#' @param args list of arguments to be passed to \code{fun}
#'
#' @export
convert_sample <- function(sample, margin, fun, args = list()){
  stopifnot(is.function(fun))
  margin <- check_margin(margin)
  for(m in margin){
    sample <- do.call(paste0("ssapply", m), args=list(s=sample, fun=fun, args=args))
  }
  return(sample)
}

check_margin <- function(m){
  if(is.character(m)){
    m <- as.numeric(sapply(m, function(x) which(x==c("obs", "var", "group"))))
  }
  if(is.numeric(m)){
    stopifnot(all(m %in% 1:3))
    stopifnot(length(m) <= 3)
    return(m)
  }
  stop("margin has wrong class")
}

ssapply1 <- function(s, fun, args){
  lapply(s, function(x) do.call(apply, c(list(X=x, MARGIN=1, FUN=fun), args)) %>%
           matrix(nrow=nrow(x), ncol=1))
}

ssapply2 <- function(s, fun, args){
  lapply(s, function(x) do.call(apply, c(list(X=x, MARGIN=2, FUN=fun), args)) %>%
           matrix(nrow=1, ncol=ncol(x)))
}

ssapply3 <- function(s, fun, args){
  a <- array(unlist(s), dim = c(nrow(s[[1]]), ncol(s[[1]]), length(s)))
  list(do.call(apply, c(list(X=a, MARGIN=1:2, FUN=fun), args)))
}

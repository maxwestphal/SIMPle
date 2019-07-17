#' @export
print.SIMPle.dist <- function(x){
  cat("A SIMPle.dist object:\n")
  str(x, 2)
}

#' @export
print.SIMPle.dlist <- function(x){
  cat("A list of SIMPle.dist objects:\n")
  str(x, 1)
}

#' @export
print.SIMPle.loss <- function(x){
  cat("A SIMPle.loss object:\n")
  str(x, 1)
}


#' @export
print.SIMPle.dist <- function(x){
  cat("A SIMPle.dist object:\n")
  str(x, 2)
}

#' @export
print.SIMPle.sample <- function(x){
  cat("A SIMPle.sample object:\n")
  x <- lapply(x, head, n=10)
  print.default(x)
}

#' @export
print.SIMPle.result <- function(x){
  cat("A SIMPle.result object:\n")
  print.default(x)
}

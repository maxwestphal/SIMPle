#' @importFrom utils str
#' @export
print.SIMPle.dist <- function(x, ...){
  cat("A SIMPle.dist object:\n")
  utils::str(x, 2)
}

#' @export
#' @importFrom utils head
print.SIMPle.sample <- function(x, ...){
  cat("A SIMPle.sample object:\n")
  x <- lapply(x, utils::head, n=10)
  print.default(x)
}

#' @export
print.SIMPle.result <- function(x, ...){
  cat("A SIMPle.result object:\n")
  print.default(x)
}

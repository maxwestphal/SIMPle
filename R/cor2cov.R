#' Convert a correlation matrix and standard deviations into a covariance matrix.
#'
#' @param rho \code{matrix}, the underlying correlation matrix.
#' @param sigma \code{numeric}, vector of standard deviations.
#'
#' @return The corresponding covariance matrix.
#' @export
#'
#' @details Copied implementation from \code{psych::cor2cov}.
cor2cov <- function(rho, sigma){
  sigma <- diag(sigma)
  cov <- sigma %*% rho %*% sigma
  colnames(cov) <- rownames(cov) <- colnames(rho)
  return(cov)
}

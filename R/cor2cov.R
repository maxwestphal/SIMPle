#' Convert a correlation matrix and standard deviations into a covariance matrix.
#'
#' @param rho \code{matrix}, the underlying correlation matrix.
#' @param sigma \code{numeric}, vector of standard deviations.
#'
#' @return The corresponding covariance matrix.
#' @export
#'
#' @details Copied implementation from \code{psych::cor2cov}, adapted to work with length(sigma)==1
cor2cov <- function(rho, sigma){
  sigma <- diag(sigma, nrow=length(sigma), ncol=length(sigma))
  cov <- sigma %*% rho %*% sigma
  colnames(cov) <- rownames(cov) <- colnames(rho)
  return(cov)
}

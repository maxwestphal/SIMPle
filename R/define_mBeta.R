#' Define a multivariate Beta (mBeta) distribution
#'
#' @param dims \code{integer}, number of dimensions
#' @param nu \code{integer}, 'pseudo-prior-observations'
#' @param mean \code{numeric} of length dims, specifying the marginal means. Repeated if of length one.
#' @param corr \code{matrix}, symetric, positive definite correlation matrix. If a single correlation
#' @param moments \code{matrix}, symmetric matrix with mixed 2nd order moments of the target distribution. See details.
#' @param gamma numeric vector, parameter of the underlying Dirichlet distribution.
#' the base 2 logarithm of length(gamma) needs to be an integer and is the dimension of the
#' resulting multivariate Beta distribution.
#' @param vars \code{character}, of length \code{dims}
#' @param mode \code{character}, either \code{"auto"} (default), \code{"full"} or \code{"reduced"}. See details.
#' @param tol \code{numeric}, maximum relative deviation (default: 0.01) between specified and
#' achieved moment matrix (or correlation matrix)
#' @param project logical (default: TRUE), should the input moment/correlation matrix be projected
#' on Frechet bounds? project=FALSE and inadmissible input will result in an error.
#' @param msg logical, enable (TRUE, default) or disable (FALSE) messages
#'
#' @return A SIMPle.dist object representing the multivariate Beta
#' distribution with given parameters.
#' @export
#'
#' @details Mode "full" implies that the full parameter vector gamma is used which results in nonzero
#' mixed moments. In mode "reduced", a sparse representation with 1+m+(m-1)*m/2 parameters is used to
#' model (prior) sample size, mean and covariance / correlation matrix. Per default (mode="auto"),
#' the mode is set to "full" for dimension <= 10 and "reduced" for dimensions >10.
#'
#' @examples define_mBeta(dims = 3, nu=10, mean=c(0.9,0.8,0.7), corr=0.5)
define_mBeta <- function(dims=2, nu=1, mean=1/2, corr=0, moments=NULL, gamma=NULL,
                         vars=NULL, mode=c("auto", "full", "reduced"), tol=0.01, project=TRUE, msg=TRUE){
  args <- as.list(environment())
  args$mode <- match.arg(mode)
  index <- which.max(c(!is.null(gamma), !is.null(moments), TRUE))
  args$input <- c("gamma", "moments", "corr")[index]
  if(args$input=="gamma"){nu <- args$nu <- sum(gamma)}
  stopifnot(length(nu) == 1 & nu >= 0)
  if(dims == 1) return(do.call(mBeta2uBeta, args))

  message("SIMPle: Defining mBeta distribution (mode: ", args$mode, ", input: ", args$input, ").")
  args$corr <- rho2corr(dims, corr)
  args$moments <- switch(args$input,
                         corr = do.call(corr2moments, args),
                         moments = moments,
                         gamma = unname(do.call(gamma2moments, args)))
  args$moments <- do.call(check_moments, args)

  return(construct_dist("mBeta", args))
}





























#' Define a multivariate beta (mbeta) distribution
#'
#' @param vars \code{integer}, number of variables
#' @param nu \code{integer}, 'pseudo-prior-observations'
#' @param mean \code{numeric} of length vars, specifying the marginal means. Repeated if of length one.
#' @param corr \code{matrix}, symetric, positive definite correlation matrix. If a single correlation
#' @param moments \code{matrix}, symmetric matrix with mixed 2nd order moments of the target distribution. See details.
#' @param gamma numeric vector, parameter of the underlying Dirichlet distribution.
#' the base 2 logarithm of length(gamma) needs to be an integer and is the dimension of the
#' resulting multivariate Beta distribution.
#' @param mode \code{character}, either \code{"auto"} (default), \code{"full"} or \code{"reduced"}. See details.
#' @param varnames \code{character}, variables names, needs to have length \code{vars}
#' @param groups \code{integer}, number of groups (subpopulations) (default: \code{1})
#' @param groupnames \code{character}, group names
#' @param tol \code{numeric}, maximum relative deviation (default: 0.01) between specified and
#' achieved moment matrix (or correlation matrix)
#' @param project \code{logical}, project parameters to satisfy Frechet bounds
#' (\code{TRUE}, default) or not (\code{FALSE})
#' @param msg \code{logical}, return messages (\code{TRUE}, default) or not (\code{FALSE})
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
#' @examples define_dist(vars = 3, nu=10, mean=c(0.9,0.8,0.7), corr=0.5)
define_dist <- function(nu=1,
                        mean=1/2,
                        corr=0,
                        moments=NULL,
                        gamma=NULL,
                        vars = length(mean),
                        varnames = NULL,
                        groups = 1,
                        groupnames = NULL,
                        mode=c("auto", "full", "reduced"),
                        tol=0.01, project=TRUE, msg=TRUE){
  args <- as.list(environment())

  index <- which.max(c(!is.null(gamma), !is.null(moments), TRUE))
  args$input <- c("gamma", "moments", "corr")[index]
  args$vars <- max(c(c(log(length(gamma), 2), nrow(moments), nrow(corr))[index], vars, length(mean)),
                   na.rm = TRUE)

  args$type <- "mbeta"
  args$mode <- get_mode_mbeta(args$vars, match.arg(mode))

  if(args$input=="gamma"){args$nu <- sum(gamma)}
  stopifnot(length(args$nu) == 1 & args$nu >= 0)

  message("SIMPle: Defining mbeta distribution (mode: ", args$mode, ", input: ", args$input, ").")
  args$corr <- rho2corr(args$vars, corr)
  args$moments <- switch(args$input,
                         corr = do.call(corr2moments, args),
                         moments = moments,
                         gamma = unname(do.call(gamma2moments, args)))
  args$moments <- do.call(check_moments, args)

  return(construct_dist(args))
}



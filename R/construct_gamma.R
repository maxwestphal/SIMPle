#' Construct the gamma parameter for the multivariate Beta (mbeta) distribution.
#'
#' @param vars integer, dimension of multivariate beta distribution.
#' @param nu numeric, equal to the sum of resulting gamma vector (prior observations).
#' @param moments matrix, .... The entry (j,k) of \code{cp} specifies the
#' expected joint probability P(B_j = B_k = 1). See details.
#' @param tol numeric, if obtained momemnt matrix deviates more than \code{tol} from the target
#' @param corr matrix, correlation matrix, only needs to be supplied if input="corr" (default: NULL)
#' @param input character, either "moments" (default) or "corr"
#' @param ...
#' a warning is given. See details.
#' @return A numeric vector gamma of length 2^dims suitable as a parameter for
#' the multivariate Beta (mbeta) distribution.
#'
#' @examples construct_gamma(3, 1)
#'
#' @export
construct_gamma <- function(vars,
                            nu=1,
                            moments=nu*(matrix(1/4, vars, vars) + diag(rep(1/4, vars))),
                            corr=NULL,
                            tol=0.01,
                            input="moments",
                            ...){
  args <- c(as.list(environment()), list(...))
  stopifnot(vars %% 1 == 0)
  if(nu==0){return(rep(0, 2^vars))}
  if(vars == 1){return(c(nu-moments, moments))}
  return(do.call(solve_gamma, args))
}

#' @importFrom limSolve lsei
solve_gamma <- function(vars, nu, moments, corr=NULL, tol=0.01, input="moments", ...){

  gamma.star <- limSolve::lsei(A=H2mat(vars),
                               B=mat2vec(moments),
                               E=rbind(Hmat(vars), 1),
                               F=c(diag(moments), nu),
                               G=diag(rep(1, 2^vars)),
                               H=rep(0, 2^vars),
                               type=1)$X

  target <- switch(input,
                   moments = moments,
                   corr = stats::cov2cor(cov_mbeta(moments=moments, nu=nu)),
                   gamma = 0)

  actual <- switch(input,
                   moments = gamma2moments(gamma.star),
                   corr = stats::cov2cor(cov_mbeta(gamma=gamma.star)),
                   gamma = 0)

  dev <- abs(actual - target); tar <- abs(target)

  if(! isTRUE(all.equal(actual, target, tol=0.01)) ){
    target.str <- switch(input, moments = "moment matrix", corr = "correlation matrix")
    warning("SIMPle: Relative deviation to desired ", target.str, " greater than tolerance (", tol, ") for at least one entry!")
    message("SIMPle: Final ", target.str, ":")
    print(target)
    print(actual)
    print(dev)
  }
  return(gamma.star)
}



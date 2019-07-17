#' Construct the gamma parameter for the multivariate Beta (mBeta) distribution.
#'
#' @param dims \code{integer}, dimension of multivariate beta distribution.
#' @param nu \code{numeric}, equal to the sum of resulting gamma vector (prior observations).
#' @param moments \code{matrix}, .... The entry (j,k) of \code{cp} specifies the
#' expected joint probability P(B_j = B_k = 1). See details.
#' @param tol \code{numeric}, if obtained momemnt matrix deviates more than \code{tol} from the target
#' a warning is given. See details.
#' @param method \code{integer}, ... either 1 (\code{lsei}) or 2 (\code{solve.QP}). Is passed to limSolve::lsei.
#'
#' @return A numeric vector gamma of length 2^dims suitable as a parameter for
#' the multivariate Beta (mBeta) distribution.
#'
#' @details [+++TODO+++] Frechet bounds! incl REF
#' CP VS MU/RHO SPEC
#' SIMPle REF
#'
#' @examples construct_gamma(3, 1)
#'
#' @export
construct_gamma <- function(dims=3, nu, moments=NULL, tol=0.01, method=1, ...){
  args <- c(as.list(environment()), list(...))
  stopifnot(dims %% 1 == 0)
  if(nu==0){return(rep(0, 2^dims))}
  return(do.call(solve_gamma, args))
}

#' @importFrom limSolve lsei
solve_gamma <- function(dims, nu, moments, corr=NULL, tol=0.01, input="moments", ...){
  #zeros <- which(apply(Hmat(m), 2, function(x) sum(x)>2)) # TODO: EXPERTIMENTAL
  #D <- diag(rep(1, 2^m))
  # theta <- diag(moments/nu)
  # gamma.star  <- limSolve::lsei(A=H2mat(dims),
  #                               B=mat2vec(moments/nu),
  #                               E=rbind(Hmat(dims), 1),
  #                               F=c(theta, 1),
  #                               G=diag(rep(1, 2^dims)),
  #                               H=rep(0, 2^dims),
  #                               type=method)$X*nu

  gamma.star <- limSolve::lsei(A=H2mat(dims),
                               B=mat2vec(moments),
                               E=rbind(Hmat(dims), 1),
                               F=c(diag(moments), nu),
                               G=diag(rep(1, 2^dims)),
                               H=rep(0, 2^dims),
                               type=1)$X

  target <- switch(input,
                   moments = moments,
                   corr = cov2cor(cov_mBeta(moments=moments, nu=nu)), #cov_mBeta(moments=moments, nu=nu)
                   gamma = 0)

  actual <- switch(input,
                   moments = gamma2moments(gamma.star),
                   corr = cov2cor(cov_mBeta(gamma=gamma.star)),
                   gamma = 0)

  #dev <- abs(actual - target); tar <- abs(target)

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






### DEVELOPMENT:


#
# gamma <- lsei(A=A, B=b, E=E, F=f, G=G, H=h, type=1)$X
# gamma;
# sum(gamma)
# gamma %>% define_mBeta() %>% {.$info$cov} %>% cov2cor()

# ### APPROACH 2:
# H = Hmat(m)
# E = rbind(rbind(H, 1), H2mat(m))
# f = c(theta, gamma0, target(m=m, theta=theta, gamma0=gamma0, corr=rho))
# G = diag(rep(1, 2^m))
# h = rep(0, 2^m)
# A = diag(rep(1, 2^m));
# b = rep(gamma0/2^m, 2^m) #???
# b
#
# gamma2 <- lsei(A=A, B=b, E=E, F=f, G=G, H=h, type=1)$X
# #gamma;
# gamma2
# sum(gamma2)
# gamma2 %>% define_mBeta() %>% {.$info$cov} %>% cov2cor()

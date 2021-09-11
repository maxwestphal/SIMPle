#' Check Fréchet bounds
#'
#' @param moments \code{matrix} a m x m square matrix, it is assumed that the diagonal gives
#' probabilies for events A_1,...,A_m and the off diagonal gives
#' @param nu \code{numeric}
#'
#' @return A list giving detailed results.
#' @export
#'
#' @details See \url{https://en.wikipedia.org/wiki/Fréchet_inequalities}.
#'
#' @importFrom matrixcalc is.symmetric.matrix
bound_moments <- function(moments, nu=1){
  stopifnot(is.matrix(moments))
  stopifnot(matrixcalc::is.symmetric.matrix(moments))
  dims <- nrow(moments)
  stopifnot(is.numeric(nu) & length(nu) == 1)
  stopifnot(all(moments <= nu))

  M <- moments/ifelse(nu==0, 1, nu); D <- diag(M); P <- M
  MR <- matrix(D, byrow=F, dims, dims)
  MC <- matrix(D, byrow=T, dims, dims)

  U <- pmin(MR, MC); diag(U) <- 1
  L <- pmax(Reduce("+", list(MR, MC))-1, 0); diag(L) <- 0

  below <- M < L; above <- M > U
  P[below] <- L[below]; P[above] <- U[above]

  list(moments = M*nu,
       projected = P*nu,
       inside = !below & !above,
       lower = L*nu,
       below = below,
       upper = U*nu,
       above = above)
}

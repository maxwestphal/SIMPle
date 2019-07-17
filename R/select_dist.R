#' Select variables of a distribution (after reordering)
#'
#' @param dist SIMPle.dist
#' @param method "mean"
#' @param cpe "min"
#' @param j prespecified ordering
#' @param m.max only select first m.max varibales according to ordering
#'
#' @return A SIMPle.dist, potentially of smaller dimension
#' @export
select_dist <- function(dist, method="mean", cpe="min", j=NULL, m.max=NULL){
  m <- dims(dist)[length(dims(dist))]
  if(is.null(j)){
    j <- order_dist(dist=dist, method=method, cpe=cpe)
  }
  if(is.null(m.max)){
    m.max <- m
  }
  if(is.SIMPle.dlist(dist)){
    return(define_dlist(dlist=lapply(dist, function(x) select_dist(x, j=j, m.max=m.max))))
  }
  D <- diag(c(rep(1,m.max), rep(0, m-m.max)))[1:m.max, ]
  P <- as.matrix(as(j, "pMatrix"))
  M <- D %*% P
  A2 <- M %*% dist$params$moments %*% t(M)
  define_mBeta(dims=m.max, nu=dist$params$nu, moments=A2)
}








#' @export
linear_contrast <- function(vs=1){
  tt <- function(dist, x=NULL, n=10000){
    m <- dims(dist)[length(dims(dist))]
    stopifnot(vs <= m)
    if(is.null(x)){x <- sample(dist, n=n)}
    K <- diag(rep(1, m))
    K[vs, ] <- -1; K <- K[,-vs]
    #R <- x %*% K
    colnames(K) <- paste0((1:m)[-vs], "vs", vs)
    return(x %*% K)
  }
  #tt <- function(x){3*x}
  return(tt)
}


#linear_contrast(3)(dist, NULL, 10)


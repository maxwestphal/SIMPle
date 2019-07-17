#' Ranking a SIMPle.dist (!experimental!)
#'
#' @param dist SIMPle.dist object
#' @param method "mean" or "prob"
#' @param cpe "min" or "prod"
#' @param cu numeric, cutoff for method "prob"
#' @param lower.tail logical, default FALSE
#'
#' @return ranking
#' @export
rank_dist <- function(dist, method=c("prob", "mean"), cpe=c("min", "prod"), cu=0.75, lower.tail=FALSE){
  method <- match.arg(method); cpe <- match.arg(cpe)
  if(is.SIMPle.dlist(dist)){
    if(length(cu)==1){cu <- rep(cu, dims(dist)[1])}
    stat <- switch(method[1],
                   mean = apply(sapply(dist, function(y)
                     sapply(y$features$margins, function(x)x$features$mean)), 1, cpe),
                   prob = apply(sapply(1:dims(dist)[1], function(l){
                     sapply(dist[[l]]$features$margins,
                            function(x) pbeta(cu[l], x$params$alpha, x$params$beta,
                                              lower.tail = lower.tail))}), 1, cpe))

  }else{
    stat <- switch(method[1],
                   mean = sapply(dist$features$margins, function(x)x$features$mean),
                   prob = sapply(dist$features$margins,
                                 function(x) pbeta(cu, x$params$alpha, x$params$beta,
                                                                          lower.tail = lower.tail)))
  }
  rank(-stat, ties.method="random")
}

# dist <- define_dlist(define_mBeta(5, 200, (18:14)/20), define_mBeta(5, 200, (15:19)/20))
# order_dist(dist)

#' Order a SIMPle.dist (!experimental!)
#'
#' @param dist SIMPle.dist object
#' @param method "mean" or "prob"
#' @param cpe "min" or "prod"
#' @param cu numeric, cutoff for method "prob"
#' @param lower.tail logical, default FALSE
#'
#' @return ordering
#' @export
order_dist <- function(dist, method="prob", cpe="min", cu=0.75, lower.tail=FALSE){
  order(rank_dist(dist=dist, method=method, cpe=cpe, cu=cu, lower.tail=lower.tail))
}





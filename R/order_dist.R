#' Order margins of a multivariate distribution
#'
#' @param dist \code{SIMPle.dist} object
#' @param method character, base order on dist parameters ("params") or sample ("sample")
#' @param target character, basis for ordering
#' @param combine character, way to combine across groups
#' @param ties.method character, see argument ties.method from function \code{base::rank}
#' @param threshold numeric, threshold values (vector of length groups(dist))
#' @param size integer, sample size(s) in predictive sample
#'
#' @export
order_dist <- function(dist,
                       method = c("param", "sample"),
                       target = c("mean", "prob", "tstat"),
                       combine = ifelse(target=="prob", "prod", "min"),
                       threshold = rep(0.75, groups(dist)),
                       size = NA,
                       ties.method="random"){
  order(rank_dist(dist=dist, method, target, combine, threshold, size, ties.method))
}

#' Rank margins of a multivariate distribution
#'
#' @param dist \code{SIMPle.dist} object
#' @param method character, base order on dist parameters ("params") or sample ("sample")
#' @param target character, basis for ordering
#' @param combine character, way to combine across groups
#' @param ties.method character, see argument ties.method from function \code{base::rank}
#' @param threshold numeric, threshold values (vector of length groups(dist))
#' @param size integer, sample size(s) in predictive sample
#'
#' @export
rank_dist <- function(dist,
                      method = c("param", "sample"),
                      target = c("mean", "prob", "tstat"),
                      combine = ifelse(target=="prob", "prod", "min"),
                      threshold = rep(0.75, groups(dist)),
                      size = NA,
                      ties.method="random"){
  method <- match.arg(method)
  target <- match.arg(target)

  stat <- do.call(paste0("rank_dist_", method),
                  args=list(dist=dist,
                            target=target, combine=combine,
                            threshold=threshold, size=size))

  ranks <- rank(-stat, ties.method=ties.method)
  names(ranks) <- varnames(dist)
  return(ranks)
}

rank_dist_sample <- function(dist, target, combine, threshold, size){
  sample <- sample_dist_marginal(dist, n=10000)
  s <- 1 # TODO: ifelse(prob_type == "greater", 1, -1)
  out <-
    switch(target,
           mean =
             sample,
           tstat =
             lapply(1:length(sample), function(g){
               tstat1(sample[[g]], threshold[g], size[g])
             }),
           prob =
             lapply(1:groups(dist), function(g){
               (s*(sample[[g]]-threshold[g])) > 0
             })
    ) %>%
    convert_sample(3, match.fun(combine)) %>%
    convert_sample(2, mean)
  return(as.numeric(out[[1]]))
}

tstat1 <- function(est, threshold, size){
  (est-threshold)/sqrt(est*(1-est)/size)
}

#' @importFrom stats pbeta
rank_dist_param <- function(dist, target, combine, threshold, size){
  out <-
    lapply(1:groups(dist), function(g){
      mom <- diag(params(dist, g)$moments)
      nu <- params(dist, g)$nu
      switch(target,
             mean = (mom/nu),
             tstat = (mom/nu-threshold[g])/sqrt(mom/nu*(1-mom/nu)/nu),
             prob = stats::pbeta(threshold[g], mom, nu-mom, lower.tail = FALSE)
      ) %>%
        matrix(nrow=1)
    })  %>%
    convert_sample(3, match.fun(combine))
  return(as.numeric(out[[1]]))
}

#' Sample from distribution ignoring the dependency structure
#'
#' @param dist a SIMPle.dist object
#' @param n integer, the sample size
#'
#' @importFrom stats rbeta
#'
#' @return \code{SIMPle.sample} object
#' @export
sample_dist_marginal <- function(dist, n=10000){
  a <- lapply(params(dist, simplify=FALSE), function(x) diag(x$moments))
  b <- lapply(params(dist, simplify=FALSE), function(x) x$nu - diag(x$moments))
  lapply(1:groups(dist), function(g){
    sapply(1:vars(dist), function(j){
      stats::rbeta(n, a[[g]][j], b[[g]][j])
    })
  }) %>%
    copy_attr(dist, c("vars", "varnames", "groups", "groupnames")) %>%
    append_class("SIMPle.sample") %>%
    return()
}

append_class <- function(x, newclass){
  class(x) <- append(class(x), newclass)
  return(x)
}

copy_attr <- function(to, from, what=NULL){
  if(is.null(what)){what <- 1:length(attributes(from))}
  mostattributes(to) <- attributes(from)[what]
  return(to)
}






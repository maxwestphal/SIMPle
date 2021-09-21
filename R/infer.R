#' Conduct statistical inference based on (posterior) sample or distribution
#'
#' @param x a \code{SIMPle.dist} or \code{SIMPle.sample} object
#' @param stats character, specifies statistics to be calculated
#' @param prob numeric, coverage probability of credible interval
#' @param aggr_var character, function name how cover events are aggregated across variables
#' @param aggr_group character, function name how cover events are aggregated across groups
#' @param method character, method to calculate credible regions
#' @param ... further arguments, e.g. passed to \code{\link{draw_sample}} if method="sample"
#'
#' @importFrom mvtnorm qmvnorm
#'
#' @return \code{SIMPle.result} object
#' @export
infer <- function(x,
                  prob = 0.95,
                  stats = "mean",
                  aggr_var = "all",
                  aggr_group = "&",
                  method = c("auto", "sample", "copula", "approx"),
                  ...){
  UseMethod("infer", x)
}

#' @export
infer.SIMPle.dist <- function(x,
                              prob = 0.95,
                              stats = "mean",
                              aggr_var = "all",
                              aggr_group = "&",
                              method = c("auto", "sample", "copula", "approx"),
                              ...){
  method <- match.arg(method)

  if(method=="auto"){
    if(groups(x) >  1){method <- "sample"}
    if(groups(x) == 1){
      method <- switch(type(x)[2],
                       full = "sample",
                       reduced = "copula")
    }
  }
  message(paste0("SIMPle: Conducting inference (method: ", method, ")"))

  args <- as.list(environment())
  args$dist <- x
  args$sample <- draw_sample(x, ..., msg=FALSE)
  args$x <- NULL

  st <- calc_stats(stats, args$sample, args$dist)
  cr <- do.call(paste0("infer_", method), args=args)
  out <- merge_results(st, cr)
  class(out) <- append(class(out), "SIMPle.result")

  return(out)
}

#' @export
infer.SIMPle.sample <- function(x,
                                prob = 0.95,
                                stats = "mean",
                                aggr_var = "all",
                                aggr_group = "&",
                                method = c("auto", "sample", "copula", "approx"),
                                ...){
  stopifnot(match.arg(method) %in% c("auto", "sample"))

  st <- calc_stats(stats, args$sample, args$dist)
  cr <- do.call(paste0("infer_", method), args=args)
  out <- merge_results(st, cr)
  class(out) <- append(class(out), "SIMPle.result")

  return(out)

}

#' @importFrom stats pnorm
infer_copula <- function(dist,
                         sample,
                         prob = 0.95,
                         stats = "mean",
                         ...){
  stopifnot(groups(dist) == 1)

  R <- stats::cov2cor(features(dist, 1)$cov)
  pr <- 1-(1-stats::pnorm(mvtnorm::qmvnorm(1-(1-prob)/2, tail="lower.tail", corr=R)$quantile))*2
  cr <- get_cr(pr, sample, dist)

  return(cr)
}

infer_approx <- function(dist,
                         sample,
                         prob = 0.95,
                         stats = "mean",
                         ...){
  stopifnot(groups(dist) == 1)

  mu <- features(dist, 1)$mean
  C <- features(dist, 1)$cov
  R <- stats::cov2cor(C)
  se <- sqrt(diag(C))
  cv <- mvtnorm::qmvnorm(1-(1-prob)/2, tail="lower.tail", corr=R)$quantile
  cr <- data.frame(lower = as.numeric(mu - cv*se),
                   upper = as.numeric(mu + cv*se))

  return(list(cr))
}

infer_sample <- function(dist,
                         sample,
                         prob,
                         stats,
                         aggr_var = "all",
                         aggr_group = "&",
                         ...){
  pr <- opt_cr(prob=prob, sample=sample, dist=dist, aggr_var=aggr_var, aggr_group=aggr_group)
  cr <- get_cr(pr, sample, dist)

  return(cr)
}

coverage <- function(pr,
                     prob,
                     sample,
                     dist = NULL,
                     aggr_var = "all",
                     aggr_group = "&"){

  n <- nrow(sample[[1]])
  m <- ncol(sample[[1]])

  cr <- get_cr(pr, sample, dist)

  E <- lapply(1:length(cr), function(g){
    (matrix(cr[[g]]$lower, nrow=n, ncol=m, byrow=TRUE) <= sample[[g]]) &
      (matrix(cr[[g]]$upper, nrow=n, ncol=m, byrow=TRUE) >= sample[[g]])
  })

  cover <- mean(apply(Reduce(aggr_group, E), 1, aggr_var))

  return(cover-prob)
}

#' @importFrom stats uniroot
opt_cr <- function(prob, sample, dist=NULL, aggr_var="all", aggr_group="&"){
  stats::uniroot(f=coverage, interval=c(1e-3, 1-1e-3),
                 prob=prob, sample=sample, dist=dist,
                 aggr_var=aggr_var, aggr_group=aggr_group)$root
}

#' @importFrom stats qbeta
#' @importFrom stats quantile
#' @importFrom stats setNames

get_cr <- function(pr, sample, dist=NULL){
  if(!is.null(dist)){
    cr <- lapply(1:groups(dist), function(g){
      a <- diag(params(dist, g)$moments)
      b <- params(dist, g)$nu - a
      data.frame(
        lower = stats::qbeta((1-pr)/2, shape1=a, shape2=b),
        upper = stats::qbeta(1-(1-pr)/2, shape1=a, shape2=b)
      )
    })
  }
  if(is.null(dist)){
    cr <- lapply(1:length(sample), function(g){
      apply(sample[[g]], 2, stats::quantile, probs=c((1-pr)/2, 1-(1-pr)/2) ) %>%
        t() %>% as.data.frame() %>% stats::setNames(c("lower", "upper"))
    })
  }
  return(cr)
}

calc_stats <- function(stats, sample, dist=NULL){
  lapply(1:length(sample), function(g) calc_stats1(g, stats, sample, dist))
}

calc_stats1 <- function(g, stats, sample, dist=NULL){
  sapply(stats, function(s) {apply(sample[[g]], 2, s)})
}

merge_results <- function(st, cr, prec=4){
  stopifnot(length(st) == length(cr))
  lapply(1:length(st), function(g){
    data.frame(lower=cr[[g]]$lower, st[[g]], upper=cr[[g]]$upper) %>% round(prec)
  })
}



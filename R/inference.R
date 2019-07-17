#' Construct a credible region for a (multivariate) probability distribution
#'
#' @param dist \code{SIMPle.dist} object.
#' @param loss \code{character}, known loss (e.g. "fwer") or self constructed loss.
#' (see \code{\link{define_loss}}).
#' @param target \code{numeric}, target value for loss, e.g. one minus coverage probability for
#' \code{loss="fwer"}.
#' @param alternative \code{character}.
#' @param control \code{list}, contains further arguments for loss.
#' @param method \code{character}, specfies the method used, see details.
#' @param type \code{character}, type of credible region, currently only \code{"equi.tail"} is supported.
#' @param n.rep \code{integer}, number of replications to draw from \code{dist} for mcmc methods
#' @param seed \code{integer}, random seed to make result replicable.
#' @param transfrom [currently unused]
#' @param threshold [currently unused]
#' @param ...
#'
#' @return A \code{SIMPle.dist} object with entry \code{inference}. Call \code{summary} or
#' \code{visualize} on output to investiagte result.
#'
#' @details [+++TODO+++]
#'
#' @export
#'
#' @examples [+++TODO+++]
inference <- function(dist,
                      loss = "fwer",
                      target = 0.05, # set zero
                      alternative = c("two.sided", "greater", "less"),
                      transfrom = NULL,
                      threshold = 0.8,
                      control = list(),
                      method = c("auto", "approx", "copula", "mcmc"),
                      type = "equi.tail",
                      n.rep = 10000,
                      seed = 1,
                      ...){
  args <- c(as.list(environment()), list(...))
  #rs <- NULL
  #if(!is.null(seed)){rs <- seed+1337}
  #set.seed(seed)
  stopifnot(is.SIMPle.dist(dist) | is.SIMPle.dlist(dist))
  args$dist <- dist.in(dist)
  args$loss <- get_loss(loss)
  stopifnot(is.numeric(target))
  args$alternative <- match.arg(alternative)
  stopifnot(is.numeric(threshold) & support(args$dist[[1]], threshold))
  stopifnot(is.list(control))
  args$loss$control = control
  args$method <- choose_infer_method(match.arg(method), args$dist[[1]])
  stopifnot(type == "equi.tail")
  stopifnot(is.numeric(n.rep))
  #print(str(args$loss))

  message("SIMPle: Constructing credible region (loss: ", args$loss$name, ", method: ", args$method, ")")
  if(!is.null(args$dist[[1]]$inference)){
    message("SIMPle: Previous inference results will be overwritten!")
  }

  # Prepare output:
  out <- list(loss = args$loss,
              details = list(target = target,
                             alternative = args$alternative,
                             threshold = threshold,
                             method = args$method,
                             type = type,
                             n.rep = ifelse(method=="copula", NA, n.rep),
                             seed = seed))

  # Add credible region(s) to result:
  result <- switch(args$method,
                   direct = do.call(direct_inference, args),
                   approx = do.call(approx_inference, args),
                   copula = do.call(copula_inference, args),
                   mcmc   = do.call(mcmc_inference,   args))

  stopifnot(length(args$dist) == length(result))
  for(p in 1:length(result)){
    args$dist[[p]]$inference <- c(out, list(result=result[[p]]))
  }
  #set.seed(NULL)
  return(dist.out(args$dist))
}






### TODO: CORRECTED PROB(H1^m) = ALL THETA BELOW theta_m???????????


direct_inference <- function(dist, ...){
  # ONLY FOR UNIVARIATE DIST!
}

approx_inference <- function(dist, alternative, target, transform=NULL, logit=FALSE,...){
  if(dims(dist)[1] != 1){stop("Approximate inference only implemented for a single distribution!")}
  dist <- dist[[1]]
  mu <- diag(dist$params$moments) / dist$params$nu
  C <- dist$features$cov;
  if(!is.null(transform)){
    mu <- transform(dist, mu)
    C <- t(transform(dist, diag(rep(1, dims(dist)[1])))) %*% transform(dist, C)
  }
  R <- cov2cor(C); se <- sqrt(diag(C))
  cv <- mvtnorm::qmvnorm(1-target/ifelse(alternative=="two.sided", 2, 1), corr=R)$quantile
  if(logit){
    se <- se / (mu*(1-mu)) / sqrt(dist$params$nu) #sqrt(abs(1/(n*estimate*(1-estimate))))
    mu <- log(mu/(1-mu))
  }
  lower <- switch(alternative, mu - cv*se, less = rep(min(dist$features$support), length(mu)))
  upper <- switch(alternative, mu + cv*se, greater = rep(max(dist$features$support), length(mu)))
  cr <- data.frame(lower = as.numeric(lower), upper = as.numeric(upper))
  rownames(cr) <- rownames(C)
  if(logit){cr <- 1/(1+exp(-cr))} # back transform
  return(list(cr))
}

copula_inference <- function(dist, alternative, target, ...){
  if(dims(dist)[1] != 1){stop("Copula inference only implemented for a single distribution!")}
  dist <- dist[[1]]
  R <- cov2cor(dist$features$cov)
  if(is.null(dist$transform)){
    qstar <- 1-pnorm(mvtnorm::qmvnorm(1-target/ifelse(alternative=="two.sided", 1, 2), corr=R)$quantile)
    cr <- get_cr(dist, qstar, alternative, NULL)
  }
  # if(!is.null(dist$transform)){
  #   rdist <- dist; rdist$type[2] <- "reduced"
  #   cr <- mcmc_inference(dist, target=target, alternative=alternative,...)[[1]]
  # }
  if(!is.null(dist$transform)){
    rdist <- dist; rdist$type[2] <- "reduced"
    K <- dist$transform(dist, x=diag(rep(1, dims(dist))))
    Rt <- round(cov2cor(t(K) %*% R %*% K), 5)
    qstar <- 1-pnorm(mvtnorm::qmvnorm(1-target/ifelse(alternative=="two.sided", 1, 2), corr=Rt)$quantile)
    S <- rdist$transform(dist)
    rdist$type <- "unknown" # TODO:
    cr <- get_cr(rdist, qstar, alternative, S)
  }
  return(list(cr))
}


define_dummy <- function(dims){
  dist <- list(type = "unknown",
               params = NA,
               featues = list(support = c(-Inf, Inf)))
  attr(dist, "dims") <- dims[length(dims)]
  class(dist) <- append(class(dist), "SIMPle.dist")
  r <- ifelse(length(dims)>1, dims[1], 1)
  dlist <- define_dlist(dlist=replicate(r, dist, simplify=F))
  return(dlist)
}


mcmc_inference <- function(dist, n.rep, seed, loss, target, alternative, transform=NULL,...){
  S <- sample(dist, method=choose_mcmc_method(dist), n=n.rep, seed=seed, msg=FALSE)
  if(is.null(transform)){transform <- dist$transform}
  if(!is.null(transform)){
    S <- lapply(switch(class(S), list(S), list=S), function(x) transform(dist, x))
    dist <- define_dummy(dims = dims(dist))
  }
  ### TODO: PROB for transform: takes marginal distributions from dist object!!!
  optim_cr(dist, S, loss, target, alternative)

}


choose_mcmc_method <- function(dist){
  if(is.SIMPle.dlist(dist)) {dist <- dist[[1]]}
  switch(dist$type[1],
         mBeta = dist$type[2],
         mNormal = "full")
}

choose_infer_method <- function(method, dist){
  if(dims(dist) == 1){
    message("SIMPle: Univariate distribution - constructing credible interval via CDF!")
    return("direct")
  }
  if(method=="auto"){
    return("mcmc") # TODO: implement useful case system here...
  }
  return(method)
}


############
##I <- base::sample(1:4000, 20)

# start <- Sys.time()
# for(i in I){
#   E <- SEPM.SYN::synthetic_analysis_batch(contrast="1", method="copula", instance=load_data(id=i))
# }
# Sys.time() - start

# 10.4394

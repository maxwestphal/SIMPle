### FUNCTIONS FOR CREDIBLE REGION OPTIMIZATION / EVALUATION

optim_cr <- function(dlist, S, loss, target, alternative, ...){
  qstar <- uniroot(f=eval_cr, interval=c(0, 0.5),
                   dlist=dlist, S=S, loss=loss, target=target, alternative=alternative)$root
  #message("qstar: ", qstar)
  cr <- lapply(1:length(dlist), function(x) get_cr(dlist[[x]], qstar, alternative, S[[x]]))
  return(cr)
}

eval_cr <- function(q, dlist, S, loss, target, alternative, ...){
  #cr <- sapply(dlist, function(x) get_cr(x, q, alternative))
  #expected_loss(S=S, lower=cr[1, ], upper=cr[2, ],
  #              loss=loss$loss, cpe=loss$cpe, aggr=loss$aggr, control=loss$control) - target
  cr <- lapply(1:length(dlist), function(x) get_cr(dlist[[x]], q, alternative, S[[x]]))
  expected_loss(S=S, lower=lapply(cr, function(x) x$lower), upper=lapply(cr, function(x) x$upper), #???????????????
                loss=loss$loss, cpe=loss$cpe, aggr=loss$aggr, control=loss$control) - target
}

get_cr <- function(dist, q, alternative, S, ...){
  m <- dims(dist); s <- support(dist)
  p  <- switch(alternative,
               less = c(min(s), 1-q),
               greater = c(q, max(s)) ,
               two.sided = c(q/2, 1-q/2))

  mp <- margin_params(dist)

  cr <- switch(dist$type[1], # CHANGE unknown to is.null(transform)
               unknown = data.frame(t(apply(S, 2, function(x) quantile(x, probs=p)))),
               mBeta = data.frame(t(sapply(1:m, function(j){ qbeta(p, mp[[j]]$alpha, mp[[j]]$beta)}))))
  colnames(cr) <- c("lower", "upper")

  return(cr)
}

find_quantiles  <- function(){
  # TODO: need to generalize above quantile finding
  # distinguish between: transform -> empirical quantiles; raw data: theoretical marginal quantiles
}


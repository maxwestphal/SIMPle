#'@export
study_acc <- function(gamma0=1,
                      mu = 0.5,
                      rho=0,
                      mode="auto",
                      adjust="",
                      loss = "fwer",
                      target = 0.025,
                      alternative = "greater",
                      method="mcmc.full",
                      n.rep = 10000,
                      instance,
                      data=NULL,
                      job =NULL){
  B <- instance$comp; m <- ncol(B); n <- nrow(B)
  check <- sapply(strsplit(paste0(as.character(as.numeric(B)), collapse=""), ""), function(x) sum(as.integer(x) * 2^(m:1-1)))
  if(gamma0 == "max1pc"){gamma0 <- min(n/100, 1)}else{
    gamma0 <- as.numeric(gamma0)
  }
  if(mu == "null"){mu <- instance$args$acc}
  dist <-
    define_mBeta(gamma=construct_gamma(m, gamma0=gamma0, mu=mu, corr=rho)) %>%
    update_dist(B, adjust=adjust) %>%
    inference(loss="fwer", target=target, alternative=alternative, method=method, n.rep=n.rep)

  cr <- dist$inference$cr; acc <- instance$args$acc; map <- sapply(dist$info$margins, function(x)x$info$mode)
  mu <- sapply(dist$info$margins, function(x)x$info$mean)

  result <- data.frame(check=check,
                       gamma0=gamma0,
                       adm.lu = all(cr$lower < cr$upper),
                       adm.mode = all(cr$lower < map) & all(cr$upper > map),
                       adm.mean = all(cr$lower < mu) & all(cr$upper > mu),
                       vol = prod(apply(cr, 1, diff)),
                       reject = sum(cr$lower > acc | cr$upper < acc ))
  result$global = result$reject > 0
  return(result)
}

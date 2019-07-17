#'@export
study_sesp <- function(nu = 2,
                       mean = 0.5,
                       corr = 0,
                       mode = "auto",
                       adjust = "",
                       loss = "fwer",
                       target = 0.025,
                       alternative = "greater",
                       method ="mcmc",
                       n.rep = 10000,
                       instance,
                       data = NULL,
                       job  = NULL){
  B <- instance$comp; m <- ncol(B[[1]]); n <- sapply(B, nrow)
  se0 <- instance$args$se; sp0 <- instance$args$sp

  check <- sapply(strsplit(paste0(as.character(as.numeric(B[[1]])), collapse=""), ""), function(x) sum(as.integer(x) * 2^(m:1-1)))

  dist <-
    define_dlist(define_mBeta(dims=m, nu=nu, mean=mean, corr=corr, mode=mode),
                 define_mBeta(dims=m, nu=nu, mean=mean, corr=corr, mode=mode)) %>%
    update(data=B, control=list()) %>%
    inference(loss=loss, target=target, alternative=alternative, method=method, n.rep=n.rep)

  cr <- lapply(dist, function(x) x$inference$result)

  reject.se <- cr[[1]]$lower > se0 | cr[[1]]$upper < se0
  reject.sp <- cr[[2]]$lower > sp0 | cr[[2]]$upper < sp0
  reject.cp <- reject.se & reject.sp

  result <- data.frame(check=check,
                       nu=nu,
                       vol.se = prod(apply(cr[[1]], 1, diff)),
                       vol.sp = prod(apply(cr[[2]], 1, diff)),
                       max.min.lower = max(pmin(cr[[1]]$lower, cr[[2]]$lower)),
                       max.min.lower.delta = max(pmin(cr[[1]]$lower - se0, cr[[2]]$lower - sp0)),
                       rej.se = sum(reject.se),
                       rej.sp = sum(reject.sp),
                       reject = any(reject.cp))
  return(result)
}



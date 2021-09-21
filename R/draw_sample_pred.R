#' Draw a sample from the predictive (mbeta-mbin) distribution
#'
#' @param dist \code{SIMPle.dist} object, see \code{\link{define_dist}}
#' @param sample either a precomputed \code{SIMPle.sample} object or an integer specifiying the sample size
#' @param size sample size(s) for predictive count data
#' @param method_ext character, method for sample 'extesnion', only "basic" available
#' @param method_pred character, for drawing predictive sample, only "rmvbin" available
#' @param regu integer, regu parameter
#' @param count logical, should outcome be counts (TRUE) or probabilities (FALSE)
#' @param msg logical, should messages be displayed? (default: TRUE)
#'
#' @importFrom bindata rmvbin
#' @importFrom extraDistr rdirichlet
#' @importFrom extraDistr rmnom
#'
#' @details Predictive mbeta-mbin distribution
#'
#' @export
draw_sample_pred <- function(dist,
                             sample=100,
                             size=rep(1, groups(dist)),
                             method_pred = c("mbeta_approx", "rmvbin"),
                             method_ext = c("basic", "cov"),
                             regu = 1,
                             count = TRUE,
                             msg = TRUE){

  method_pred <- match.arg(method_pred)
  method_ext <- match.arg(method_ext)

  stopifnot(length(size) == groups(dist))
  stopifnot(type(dist)[2] %in% c("full", "reduced"))

  if(type(dist)[2] == "full"){
    if(msg){
      message("SIMPle: Drawing predictive (mbeta-mbin) sample (method: Dirichlet-multinomial)")
    }
    n <- ifelse("SIMPle.sample" %in% class(sample), nrow(sample[[1]]), sample)
    sample_pred <- lapply(1:groups(dist), function(g){
      sample_ext <- extraDistr::rdirichlet(n, normalize_gamma(params(dist, g)$gamma))
      extraDistr::rmnom(n=rep(1, n), size=rep(size[g], n), prob=sample_ext) %*%
        t(Hmat(vars(dist)))
    })

  }

  if(type(dist)[2] == "reduced"){
    sample_ext <- draw_sample_ext(dist, sample, method_ext, msg)
    if(msg){
      message(paste0("SIMPle: Drawing predictive (mbeta-mbin) sample (method: ", method_pred, ")"))
    }
    sample_pred <- lapply(1:groups(dist), function(g){
      switch(method_pred,
             rmvbin = draw_sample_mbin1(g, sample_ext, size, regu),
             mbeta_approx = draw_sample_mbeta1(g, sample_ext, size, regu))
    })
  }

  if(!count){sample_pred <- lapply(1:groups(dist), function(g){sample[[g]]/size[g]} )}

  return(sample_pred)
}

normalize_gamma <- function(gamma){
  if(any(gamma==0)){
    w <- length(gamma)
    nu <- sum(gamma)
    gamma <- 999/1000 * gamma + 1/1000 * rep(nu/w, w)
  }
  return(gamma)
}

draw_sample_mbeta1 <- function(g, sample_ext, size, regu=1){
  vars <- nrow(sample_ext[[1]][[1]])
  mom <- matrix(regu/4, vars, vars)+diag(rep(regu/4, vars))
  lapply(sample_ext[[g]], function(cp){
    quiet(
      return(
        define_dist(nu=size[g]+regu, moments = mom+cp*size[g], mode="reduced", msg =FALSE) %>%
          draw_sample(n=1, msg=FALSE) %>% as.data.frame() %>%
          prob_postproc(size[g], count=TRUE, correct=TRUE)
      ),
      all=TRUE)
  }) %>%
   do.call(rbind, .data) %>% unname() %>% as.matrix()
}

prob_postproc <- function(prob, n=1, count=FALSE, correct=FALSE){
  if(!count & !correct){return(prob)}
  if( count & !correct){return(prob*n)}
  if( count &  correct){return(round(prob*n))}
  if(!count &  correct){return(round(prob*n)/n)}
}

draw_sample_mbin1 <- function(g, sample_ext, size, regu=1){
  vars <- nrow(sample_ext[[1]][[1]])
  mom <- matrix(regu/4, vars, vars)+diag(rep(regu/4, vars))
  lapply(sample_ext[[g]], function(cp){
    quiet(
      return(
        colMeans(bindata::rmvbin(size[g], commonprob = (cp*vars+mom)/(vars+regu) ))
        ),
      all=TRUE
    )
  }) %>%
    do.call(rbind, .data)
}

draw_sample_ext <- function(dist, sample=100, method_ext = "basic", msg=TRUE){
  if(msg){message(paste0("SIMPle: Extending mbeta sample (method: ", method_ext, ")"))}
  stopifnot(any(c("numeric", "SIMPle.sample")  %in% class(sample)))
  if(is.numeric(sample)){
    stopifnot(sample %% 1 == 0)
    sample <- draw_sample(dist, n=sample, msg = msg)
  }
  stopifnot(length(sample) == groups(dist))
  lapply(1:groups(dist), function(g) {
    draw_sample_ext1(params(dist, g)$moments, params(dist, g)$nu, sample[[g]], method_ext)
  })
}

draw_sample_ext1 <- function(mom, nu, sample, method_ext = "basic"){
  if(method_ext == "basic"){
    b0 <- bound_moments(moments=mom, nu=nu)
    w0 <- get_weights(b0)
    out <- lapply(1:nrow(sample), function(i) calc_moments(sample[i, ], w0))
  }
  if(method_ext == "cov"){
    R <- cov_mbeta(nu=nu, moments = mom) %>% stats::cov2cor()
    out <- lapply(1:nrow(sample), function(i){
      theta <- sample[i, ]
      V <- diag(theta*(1-theta)/2)
      C <- V^0.5 %*% R %*% V^0.5
      make_symmetric(2*C+theta %*% t(theta))
    })
  }
  return(out)
}

make_symmetric <- function(M){
  (M + t(M))/2
}

get_weights <- function(b){
  w <- (b$moments - b$lower) / (b$upper - b$lower)
  w <- (w<=1 & w >= 0)*w + (w < 0)*0 + (w>1)*1
  return(w)
}

calc_moments <- function(theta, w0){
  stopifnot(all(length(theta)==dim(w0)))
  b <- bound_moments(diag(theta), 1)
  mom1 <- b$lower + w0*(b$upper-b$lower)
  diag(mom1) <- theta
  return(mom1)
}

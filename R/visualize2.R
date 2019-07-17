#' @importFrom colorRamps matlab.like2
quantiles_cdf <- function(S, probs=NULL, ...){
  m <- ncol(S)
  if(is.null(probs)){
    if(m == 1){probs <- 0.5}
    if(m == 2){probs <- 0:1}
    if(m %in% 3:4){probs <- (0:2)/2}
    if(m >= 5){probs <- (0:4)/4}
  }
  cols <- colorRamps::matlab.like2(length(probs))
  Q <- apply(S, 1, function(x) quantile(x, probs))
  for(j in 1:length(probs)){
    ecdf(Q[j,]) %>% plot(add=j>1, xlim=c(0,1), ylim=c(0,1), col=cols[j], lwd=2,
                         xlab=bquote(bold(vartheta[p])), ylab=bquote(F(vartheta[p])),
                         main = "Quantiles of mBeta distribution (CDF)")
  }
}

extremes_hist <- function(S, ...){
  old.par <- par(no.readonly = TRUE)
  cols <- colorRamps::matlab.like2(5)
  br <- seq(0,1,0.05)
  Q <- apply(S, 1, range)
  lim <- apply(Q, 1, function(x) table(cut(x, breaks=br))) %>% max()/0.05/nrow(S)
  #dev.new()
  par(mfrow=c(2,1))
  par(mar=c(0,5,3,3))
  hist(Q[2, ], main="Extremes of mBeta distribution (histogram)" , freq=F, xlim=c(0, 1),
       ylab=bquote( max ),
       xlab="", ylim = c(0, lim),
       xaxt="n", las=1 , col=cols[5], breaks=seq(0,1,0.05))
  par(mar=c(5,5,0,3))
  hist(Q[1, ], main="" , freq=F, xlim=c(0, 1), ylab=bquote(min),
       xlab = bquote(vartheta), ylim = c(lim, 0),
       las=1 , col=cols[1], breaks=seq(0,1,0.05))
  par(old.par)
  #dev.off()
}

fill <- function(vals, base){
  b <- rep(0, length(base)); names(b) <- base
  for(i in names(b)){b[i] <- ifelse(is.na(vals[i]), 0, vals[i])}
  return(b)
}

excount <- function(S, x, dir="greater"){
  fill(vals = switch(dir,
              greater = table(apply(S, 1, function(z) sum(z>x))),
              less    = table(apply(S, 1, function(z) sum(z<x))))/nrow(S),
       base = 0:ncol(S))
}



excount_dist <- function(S, x = c(0.8, 0.85, 0.9), dir=c("greater", "less"), ...){
  dir <- match.arg(dir)
  comp <- switch(dir, greater = ">", less = "<")
  old.par <- par(no.readonly = TRUE)
  cols <- colorRamps::matlab.like2(length(x))
  vals <- lapply(x, function(y) excount(S, y, dir))
  par(mfrow=c(length(x),1))

  #par(mar=c(0,5,3,3))
  for(i in 1:length(x)){
    barplot(vals[[i]], col = cols[i], main=bquote(bold(P("#"*(vartheta * .(comp) * .(x[i]) ) == k ))))
  }
  par(old.par)
}



#SEPM.SYN:::DIST[["conservative_blocks1"]] %>% sample() %>% excount_dist()
#SEPM.SYN:::DIST[["liberal_blocks1"]] %>% sample() %>% excount_dist()

#SEPM.SYN:::DIST[["conservative_blocks1"]] %>% visualize(1)


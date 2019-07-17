# TODO: add CRs from dist object
# TODO: choose correct density function
# TODO: choose xlims more eleborate

# lwd = 1.5; digits=2; cor.cex=8; pch=20; means = TRUE; smoother = TRUE; points.show = FALSE; points.cex=1.5;
# cr = FALSE; alpha=0.05; labels=NULL; labels.pos = c(0.15, 0.85); labels.cex=4; labels.font=2;
# lm.show=TRUE; limits = "supp"; axis.cex=2.5; jiggle = FALSE

pairs_dist <- function(dist, S, lwd = 1.5, digits=2, cor.cex=8,
                       pch=20, means = TRUE, smoother = TRUE, points.show = FALSE, points.cex=1.5,
                       cr = FALSE, alpha=0.05,
                       labels=NULL, labels.pos = c(0.15, 0.85), labels.cex=4, labels.font=2,
                       lm.show=TRUE, limits = "supp", axis.cex=2.5,
                       jiggle = FALSE, ...){

  #ci = cr
  # dist <- define_mBeta(dims=3, nu=20, mean=c(0.8,0.775,0.75), corr=0.5)
  m <- dims(dist); J <- 1:m
  #S <- sample(dist)
  R <- cov2cor(dist$features$cov)

  xr  <- switch(limits,
                supp = sapply(dist$features$margins, function(x) x$features$supp) )

  ylim <- xlim <- c(min(xr[1, ]), max(xr[2, ]))
  xx <- seq(xlim[1], xlim[2], length.out = 1000)
  dfun <- dbeta
  DD <- sapply(dist$features$margins, function(mar) dfun(xx, mar$params$alpha, mar$params$beta))
  ddmax <- max(DD)
  DD <- DD / ddmax
  pm <- sapply(dist$features$margins, function(mar) mar$features$mean)
  sd <- sqrt(sapply(dist$features$margins, function(mar) mar$features$var))

  if(is.null(labels)){
    labels <- vars(dist)
  }

  "panel.hist.density" <- function(x, ...) {
    # usr <- par("usr")
    # on.exit(par(usr))
    # par(usr = c(usr[1], usr[2], 0, 1.5))

    mar <- dist$features$margins[[x]]
    dd <- DD[, x]
    lines(xx, dd, lwd=lwd, xlab='')
    polygon(xx, dd, col='skyblue')
    lines(c(pm[x], pm[x]), c(0, dfun(pm[x], mar$params$alpha, mar$params$beta)/ddmax), col = "red", lty = 2, lwd = lwd*2)
  }
  "panel.cor" <- function(x, y, prefix = "", ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- R[x, y]

    txt <- format(c(round(r, digits), 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")

    cex <- cor.cex # cex.cor * 0.8/(max(strwidth("0.12***"), strwidth(txt)))
    text(0.5, 0.5, txt, cex = cex)
    # if (scale) {
    #   cex1 <- cex * abs(r)
    #   if (cex1 < 0.25)
    #     cex1 <- 0.25
    #   text(0.5, 0.5, txt, cex = cex1)
    # }
    # else {
    #   text(0.5, 0.5, txt, cex = cex)
    # }
  }
  "panel.lm" <- function(x, y, pch = par("pch"), col.lm = "red",
                         ...) {
    #x <- S[,x]; y <- S[,y]

    # ymin <- min(S[,y])
    # ymax <- max(S[,y])
    # xmin <- min(S[,x])
    # xmax <- max(S[,x])
    # ylim <- c(min(ymin, xmin), max(ymax, xmax))
    # xlim <- ylim
    # if (jiggle) {
    #   x <- jitter(x, factor = factor)
    #   y <- jitter(y, factor = factor)
    # }
    if (smoother) {
      smoothScatter(S[,x], S[,y], add = TRUE, nrpoints = 0, nbin=150, xlim=xlim, ylim=ylim)
    }
    else {
      if (points.show) {
        points(S[,x], S[,y], pch = pch, ylim = ylim, xlim = xlim, cex=points.cex,
               ...)
      }
    }
    if (means) {
      points(pm[x], pm[y], col="red", pch=18, cex=points.cex*2)
    }
    ok <- is.finite(S[,x]) & is.finite(S[,y])
    if (any(ok)) {
      #lml <- lm(S[,y] ~ S[,x])
      # if (ci) {
      #   tempx <- data.frame(x = seq(min(x, na.rm = TRUE),
      #                               max(x, na.rm = TRUE), length.out = 47))
      #   pred <- predict.lm(lml, newdata = tempx, se.fit = TRUE)
      #   upperci <- pred$fit + confid * pred$se.fit
      #   lowerci <- pred$fit - confid * pred$se.fit
      #   polygon(c(tempx$x, rev(tempx$x)), c(lowerci,
      #                                       rev(upperci)), col = adjustcolor("light grey",
      #                                                                        alpha.f = 0.8), border = NA)
      # }
      # if (ellipses) {
      #   xm <- mean(x, na.rm = TRUE)
      #   ym <- mean(y, na.rm = TRUE)
      #   xs <- sd(x, na.rm = TRUE)
      #   ys <- sd(y, na.rm = TRUE)
      #   r = cor(x, y, use = "pairwise", method = "pearson")
      #   draw.ellipse(xm, ym, xs, ys, r, col.smooth = "red",
      #                ...)
      # }
      if(lm.show){
        b = R[x, y] * sd[y] / sd[x]; a = pm[y] - b*pm[x]
        abline(a=a, b=b, col = col.lm, lwd=lwd, ...)
      }
    }
  }
  "label.panel" <- function(x, y=NULL, labels, cex, font, ...){
    text(labels.pos[1], labels.pos[2], labels, font=labels.font, cex=labels.cex)
  }
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))

  # if (missing(cex.cor))
  #   cex.cor <- 1
  # for (i in 1:ncol(x)) {
  #   if (is.character(x[[i]])) {
  #     x[[i]] <- as.numeric(as.factor(x[[i]]))
  #     colnames(x)[i] <- paste(colnames(x)[i], "*", sep = "")
  #   }
  # }

  #n.obs <- nrow(x)
  #confid <- qt(1 - alpha/2, n.obs - 2)
  pairs2(x=t(J),
         labels = labels,
         xlim=xlim, ylim=ylim,
         diag.panel = panel.hist.density,
         upper.panel = panel.cor,
         lower.panel = panel.lm,
         text.panel = label.panel,
         pch = pch,
         cex.axis = axis.cex,
         las=1,
         ...)
}


### ggplot variant:

# require(ggplot2)
# require(GGally)
#
# ?ggpairs






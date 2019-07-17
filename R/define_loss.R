#' Define a loss
#'
#' @param name \code{character}, name of the loss. All other arguments are ignored if the name
#' for a known loss
#' @param loss \code{function}, needs to have formal arguments [+++ TODO +++].
#' @param cpe \code{function}, specifies if co-primary enpoints ...
#' @param max \code{function}, specifies how per variable loss is aggregated over variables
#' @param control \code{list}, can be used to specify default arguments of lossfun.
#'
#' @return A \code{SIMPle.loss} object, may be used for statistical \code{\link{inference}}.
#' @export
#'
#' @examples [+++ TODO +++]
define_loss <- function(name = "fwer",
                        loss = fp,
                        cpe = sum,
                        aggr = max,
                        control = list()){
  stopifnot(is.character(name))
  if(name %in% names(LOSS)){
    message("Regcognized loss ", name, " - using corresponding SIMPle.loss.")
    return(LOSS[[name]])
  }
  loss <- list(name=name,
               loss = check_lossfun(loss),
               cpe = check_cpefun(cpe),
               aggr = check_aggrfun(aggr),
               control = check_lossctrl(control))
  class(loss) <- append(class(loss), "SIMPle.loss")
  return(loss)
}



linint <- function(a, b, y=c(0,1)){
  function(x){ pmin(pmax((x-a)/(b-a), y[1]), y[2]) }
}

linloss <- function(S, L, U, a=0.75, b=0.95, c0=1/2, c1=1, ...){ # DEFAULT VALS NEED TO BE SUPPLIED IN control from define; can be defined in control
  li <- linint(a, b)
  O <- S < L | S > U; I <- !O
  D <- do.call(li, list(S))
  # overall costs:
  (I * c0 * D - O * c1 * D)
}

#https://www.math.kth.se/matstat/gru/Statistical%20inference/Lecture9.pdf
binloss <- function(S, L, U, c0, c1,...){
  O <- S < L | S > U; I <- !O
  (I * c0 - O * c1) # minus => search root / plus => search min?!?
}

fp <- function(S, L, U, ...){ #
  S < L | S > U
}

b2B <- function(b, m, n){
  matrix(b, ncol=m, nrow=n, byrow=T)
}


expected_loss <- function(S, lower, upper, loss=fp, cpe="+", aggr=function(x){max(x)>0}, control=list()){
  #if(!is.list(S)){S <- list(S); upper <- list(upper); lower=list(lower)}
  S <- list.in(S); lower <- list.in(lower); upper <- list.in(upper)

  m <- ncol(S[[1]]); n <- nrow(S[[1]]); len <- length(S)

  L <- lapply(lower, function(x) b2B(x, m, n))
  U <- lapply(upper, function(x) b2B(x, m, n))

  # Iterate over all subpopulations:
  EL <- lapply(1:len, function(p) do.call(loss, c(list(S=S[[p]], L=L[[p]], U=U[[p]]), control) ))

  # Aggregate into single cost matrix
  EA <- Reduce(cpe, EL) #do.call(cpe, EL) #Reduce(cpe, EL) #Reduce("+", EL) #do.call(cpe, EL)

  return(mean(apply(EA, 1, aggr)))  # apply(EA, 1, aggr) # do.call(apply, c(list(X=EA, MARGIN=1, FUN=aggr)
}

get_loss <- function(loss){
  if(is.SIMPle.loss(loss)){return(loss)}
  if(is.character(loss)){
    if(!loss %in% names(LOSS)){stop(paste0("Unknown loss: ", loss))}
    return(LOSS[[loss]])
  }
}

LOSS <- list()
LOSS[["fwer"]] <- define_loss(name = "fwer", loss = fp, cpe="+", aggr=function(x){max(x)>0},
                              control = list())
LOSS[["fwer2"]] <- define_loss(name = "fwer2", loss = fp, cpe="+", aggr=function(x){max(x)==2},
                              control = list())
LOSS[["binary"]] <- define_loss(name = "binary", loss = fp, cpe="+", aggr=max,
                                control = list(c0=1/2, c1=1))
LOSS[["linear"]] <- define_loss(name = "linear", loss=linloss, cpe="+", aggr=max,
                                control = list(a=0.75, b=0.95, c0=1/2, c1=1))




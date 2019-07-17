check_lossfun <- function(loss){ # TODO
  stopifnot(is.function(loss))
  loss
}

check_cpefun <- function(cpe){ # TODO
  #stopifnot(is.function(cpe))
  cpe
}

check_aggrfun <- function(aggr){ #TODO
  stopifnot(is.function(aggr))
  aggr
}

check_lossctrl <- function(lossctrl){ # TODO
  stopifnot(is.list(lossctrl))
  lossctrl
}

check_lossargs <- function(sample, lower, upper, cpe=TRUE, ...){ # NEEDED?? WHERE??
  args <- c(as.list(environment()), list(...))
  list.input <- is.list(sample) & is.list(lower) & is.list(upper)
  numeric.input <- is.matrix(sample) & is.numeric(lower) & is.numeric(upper)
  stopifnot(list.input | numeric.input)
  if(!cpe & list.input){
    stop("SIMPle: Co-primary endpoint analysis not defined for this loss!")
  }
  if(list.input){
    stopifnot(all(c(is.list(sample), is.list(lower), is.list(upper))))
    stopifnot(all.equal(c(length(sample), length(lower), length(upper))))
    stopifnot(all.equal(c(sapply(sample, ncol), sappy(lower, length), sapply(upper, length))))
  }
  if(numeric.input){
    args$sample <- list(sample); args$lower <- list(lower); args$upper <- list(upper)
  }
  return(args)
}

### EXPORTED UTILITY FUNCTIONS:
## CLASS CHECKS: is
#' @export
is.SIMPle.dist <- function(x){
  inherits(x, "SIMPle.dist")
}

#' @export
is.SIMPle.dlist <- function(x){
  inherits(x, "SIMPle.dlist")
}

#' @export
is.SIMPle.loss <- function(x){
  inherits(x, "SIMPle.loss")
}

is.SIMPle.transfrom <- function(x){
  inherits(x, "SIMPle.transform")
}

## VARIABLE NAMES: vars
#' @export
vars <- function(x){
  UseMethod("vars", x)
}

#' @export
vars.SIMPle.dist <- function(x){
  attr(x, "vars")
}

#' @export
vars.SIMPle.dist <- function(x){
  attr(x, "vars")
}


### DIMENSION(S): dims
#' @export
dims <- function(x){
  UseMethod("dims", x)
}

#' @export
dims.SIMPle.dist <- function(x){
  attr(x, "dims")
}

#' @export
dims.SIMPle.dlist <- function(x){
  attr(x, "dims")
}




##### BELOW: NEEDED!!!
#' Extract information
#'
#' @param dist \code{SIMPle.dist}
#' @param attr \code{character}
#'
#' @return [+++TODO+++]
#' @export
#'
#' @examples [+++TODO+++]
get <- function(dist, attr="type"){ ############## TODO: BROKEN!!!
  UseMethod("get", dist)
}

get.SIMPle.dist <- function(dist, attr="type"){
  ul <- unlist(dist, recursive = FALSE)
  index1 <- names(dist) %in% attr
  index2 <- names(ul) %in% paste0(".", attr)
  index <- c(index1, index2)
  if(!any(index)){stop("No matching attribute '", attr, "' found!" )}
  if(sum(index) > 1){warning("Multiple matching attributes '", attr, "' found! Returning only the first one!")}
  return(c(dist, ul)[[min(which(index))]])
}

#' @export
params <- function(dist){
  dist$params
}

#' @export
features <- function(dist){
  dist$features
}

#' @export
margins <- function(dist){
  dist$features$margins
}

#' @export
margin_params <- function(dist){
  lapply(margins(dist), params)
}

#' @export
support <- function(dist, x=NULL){
  supp <- features(dist)$support
  if(!is.null(x)){
    return(all(x >= min(supp) & x <= max(supp)))
  }
  return(supp)
}


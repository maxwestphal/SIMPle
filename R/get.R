#' Return type of distribution
#'
#' @param x a \code{SIMPle.dist} object
#'
#' @export
type <- function(x){
  UseMethod("type", x)
}

#' @export
type.SIMPle.dist <- function(x){
  attr(x, "type")
}

#' @export
type.SIMPle.sample <- function(x){
  attr(x, "type")
}

#' Return support of distribution
#'
#' @param x a \code{SIMPle.dist} object
#'
#' @export
support <- function(x){
  UseMethod("support", x)
}

#' @export
support.SIMPle.dist <- function(x){
  attr(x, "support")
}

#' Return number of variables of distribution
#'
#' @param x a \code{SIMPle.dist} object
#'
#' @export
vars <- function(x){
  UseMethod("vars", x)
}

#' @export
vars.SIMPle.dist <- function(x){
  attr(x, "vars")
}

#' @export
vars.SIMPle.sample <- function(x){
  attr(x, "vars")
}

#' Return variables names of distribution
#'
#' @param x a \code{SIMPle.dist} object
#'
#' @export
varnames <- function(x){
  UseMethod("varnames", x)
}

#' @export
varnames.SIMPle.dist <- function(x){
  attr(x, "varnames")
}

#' @export
varnames.SIMPle.sample <- function(x){
  attr(x, "varnames")
}

#' Return number of (sub)groups of distribution
#'
#' @param x a \code{SIMPle.dist} object
#'
#' @export
groups <- function(x){
  UseMethod("groups", x)
}

#' @export
groups.SIMPle.dist <- function(x){
  attr(x, "groups")
}

#' @export
groups.SIMPle.sample <- function(x){
  attr(x, "groups")
}

#' Return groupnames of distribution
#'
#' @param x a \code{SIMPle.dist} object
#'
#' @export
groupnames <- function(x){
  UseMethod("groupnames", x)
}

#' @export
groupnames.SIMPle.dist <- function(x){
  attr(x, "groupnames")
}

#' @export
groupnames.SIMPle.sample <- function(x){
  attr(x, "groupnames")
}

#' Return type of distribution
#'
#' @param dist a \code{SIMPle.dist} object
#' @param groups integer, restrict query to (sub)group
#' @param simplify logical, should output be simplified if length(groups)==1 (default: TRUE)
#'
#' @export
params <- function(dist, groups=1:length(dist), simplify=TRUE){
  UseMethod("params", dist)
}

#' @export
params.SIMPle.dist <- function(dist, groups=1:length(dist), simplify=TRUE){
  out <- lapply(dist[groups], function(x) x$params)
  if(length(out) == 1 & simplify){
    return(out[[1]])
  }
  return(out)
}

#' Return features of distribution
#'
#' @param dist a \code{SIMPle.dist} object
#' @param groups integer, restrict query to (sub)group
#' @param simplify logical, should output be simplified if length(groups)==1 (default: TRUE)
#'
#' @export
features <- function(dist, groups=1:length(dist), simplify=TRUE){
  UseMethod("features", dist)
}

#' @export
features.SIMPle.dist <- function(dist, groups=1:length(dist), simplify=TRUE){
  out <- lapply(dist[groups], function(x) x$features)
  if(length(out) == 1 & simplify){
    return(out[[1]])
  }
  return(out)
}

#' Return marginal parameters of distribution
#'
#' @param dist a \code{SIMPle.dist} object
#' @param groups integer, restrict query to (sub)group
#' @param simplify logical, should output be simplified if length(groups)==1 (default: TRUE)
#'
#' @export
margin_params <- function(dist, groups=1:length(dist), simplify=TRUE){
  UseMethod("margin_params", dist)
}


#' @export
margin_params.SIMPle.dist <- function(dist, groups=1:length(dist), simplify=TRUE){
  DIST[[type(dist)[1]]]$margin_params(dist, groups, simplify)
}







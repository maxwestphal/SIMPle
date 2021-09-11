#' @export
type <- function(x){
  UseMethod("type", x)
}

#' @export
type.SIMPle.dist <- function(x){
  attr(x, "type")
}

#' @export
support <- function(x){
  UseMethod("support", x)
}

#' @export
support.SIMPle.dist <- function(x){
  attr(x, "support")
}

#' @export
vars <- function(x){
  UseMethod("vars", x)
}

#' @export
vars.SIMPle.dist <- function(x){
  attr(x, "vars")
}

#' @export
varnames <- function(x){
  UseMethod("varnames", x)
}

#' @export
varnames.SIMPle.dist <- function(x){
  attr(x, "varnames")
}

#' @export
groups <- function(x){
  UseMethod("groups", x)
}

#' @export
groups.SIMPle.dist <- function(x){
  attr(x, "groups")
}

#' @export
groupnames <- function(x){
  UseMethod("groupnames", x)
}

#' @export
groupnames.SIMPle.dist <- function(x){
  attr(x, "groupnames")
}

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

#' @export
margin_params <- function(dist, which=1:length(dist), simplify=TRUE){
  UseMethod("margin_params", dist)
}


#' @export
margin_params.SIMPle.dist <- function(dist, which=1:length(dist), simplify=TRUE){
  DIST[[type(dist)[1]]]$margin_params(dist, which, simplify)
}







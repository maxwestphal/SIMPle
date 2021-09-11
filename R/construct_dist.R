construct_dist <- function(a){
  stopifnot(is.numeric(a$vars) & is.numeric(a$groups))
  stopifnot(a$vars %% 1 == 0)
  stopifnot(a$groups %% 1 == 0)
  stopifnot(length(a$varnames) %in% c(0, a$vars))
  stopifnot(length(a$groupnames) %in% c(0, a$groups))
  D <- DIST[[a$type]]
  d <- list(params    = do.call(D$params, a),
            features  = do.call(D$features, a))
  dist <- replicate(a$groups, d, simplify=FALSE)
  names(dist) <- a$groupnames
  class(dist) <- append(class(dist), "SIMPle.dist")
  attr(dist, "type") <- c(a$type, a$mode)
  attr(dist, "support") <- D$support
  attr(dist, "vars") <- a$vars
  attr(dist, "varnames") <- a$varnames
  attr(dist, "groups") <- a$groups
  attr(dist, "groupnames") <- a$groupnames
  return(dist)
}







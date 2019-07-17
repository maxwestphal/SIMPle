construct_dist <- function(type, args){
  D <- DIST[[type]]
  args$mode <- do.call(D$mode, args)
  dims <- get_dims(args, D$dim.range)
  dist <- list(type      = c(type, args$mode),
               params    = do.call(D$params, args),
               features  = do.call(D$features, args),
               transform = NULL,
               inference = NULL)
  class(dist) <- append(class(dist), "SIMPle.dist")
  attr(dist, "vars") <- get_vars(args, dims)
  attr(dist, "dims") <- dims
  return(dist)
}

get_dims <- function(args, dim.range){
  stopifnot(args$dims %% 1 == 0)
  stopifnot(args$dims >= min(dim.range) & args$dims <= max(dim.range))
  return(args$dims)
}

get_vars <- function(args, m){
  vars <- args$vars
  stopifnot(length(vars) %in% c(0, m))
  if(is.null(vars)) {vars <- paste0("V", 1:m)}
  return(vars)
}



# get_info <- function(type, args, dim, D){
#   list(type = type,
#        dim = dim,
#        mode = D$mode(args),
#        support = D$support)
# }




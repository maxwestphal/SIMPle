#' Select and reorder variables of a distribution
#'
#' @param dist \code{SIMPle.dist} object
#' @param selection integer vector, describes new variable order/selection
#' @param vars number of variables to select after ordering
#' @param args list, named arguments passed to order_dist
#'
#' @return A \code{SIMPle.dist} object, potentially of smaller dimension
#' @export
select_vars <- function(dist,
                        selection = NULL,
                        vars = NULL,
                        args = list()){

  if(is.null(selection)){
    stopifnot(is.numeric(vars) & vars %% 1 == 0 & vars >= 1)
    selection <- do.call(order_dist, c(list(dist=dist), args))
  }

  if(is.null(vars)){
    vars <- length(selection)
  }

  if(length(selection) < vars(dist)){
    selection <- c(selection, (1:vars(dist))[! 1:vars(dist) %in% selection])
  }

  stopifnot(length(selection) == vars(dist))
  stopifnot(all(selection %in% 1:vars(dist)))

  if(all(selection == 1:vars(dist)) & vars==vars(dist)){return(dist)}

  out <- dist
  for(g in 1:groups(out)){

    D <- diag(c(rep(1, vars), rep(0, vars(dist)-vars)))[1:vars, ]
    P <- as.matrix(as(selection, "pMatrix"))
    M <- D %*% P

    out[[g]]$params$moments <- M %*% dist[[g]]$params$moments %*% t(M)
    out[[g]]$params$gamma <- NA
    out[[g]]$features$mean <- diag(out[[g]]$params$moments) / out[[g]]$params$nu
    out[[g]]$features$cov <- cov_mbeta(nu=out[[g]]$params$nu, moments=out[[g]]$params$moments)
  }
  if(vars == 1){attr(dist, "type") <- "beta"}
  if(vars >= 1){attr(dist, "type") <- c("mbeta", "reduced")}
  attr(out, "vars") <- vars
  attr(out, "varnames") <- varnames(dist)[selection]
  return(out)
}

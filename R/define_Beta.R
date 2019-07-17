#' Define a univariate Beta distribution.
#'
#' Define a \code{SIMPle} representation of a univariate Beta distribution via its two parameters.
#'
#' @param alpha \code{numeric}, first shape parameter \eqn{\alpha \ge 0}.
#' @param beta \code{numeric}, second shape parameter \eqn{\beta \ge 0}.
#'
#' @return A \code{SIMPle.dist} object representing the univariate Beta
#' distribution with given parameters alpha and beta.
#'
#' @details A extensive overview over the univariate Beta distribution is given at
#' \url{https://en.wikipedia.org/wiki/Beta_distribution}.
#'
#' @examples define_Beta(1/2, 1/2) # "Jeffrey's prior distribution"
#'
#' @export
define_Beta <- function(alpha=1, beta=1, vars=NULL){
  args <- c(as.list(environment()), list(dims=1))
  # if(length(alpha) > 1){
  #   alpha <- as.list(alpha); beta <- as.list(beta)
  #   dl <- lapply(1:length(alpha), function(p) define_Beta(alpha[[p]], beta[[p]], name))
  #   return(define_dlist(dlist = dl))
  # }
  construct_dist(type="Beta", args=args)
}

# define_Beta.numeric <- function(alpha=1, beta=1, name=NULL){
#   args <- c(as.list(environment()), list(dim=1))
#   stopifnot(length(alpha) == length(beta))
#   if(length(alpha) > 1){
#     alpha <- as.list(alpha); beta <- as.list(beta)
#     return(construct_Beta(alpha, beta, name))
#   }
#   construct_dist(type="Beta", args=args)
# }


# define_Beta.list <- function(alpha, beta, name=NULL){
#   dlist <- lapply(1:length(alpha), function(p) define_Beta(alpha[[p]], beta[[p]], name))
#   return(create_dlist(dlist=dlist))
# }





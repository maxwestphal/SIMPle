dir2mbeta <- function(x){
  if(!is.matrix(x)){x <- matrix(x, nrow=1)}
  m <- log(ncol(x), 2); stopifnot(m %% 1 == 0)
  x %*% t(Hmat(m))
}

#' @importFrom methods as
Hrow <- function(j, m, class="integer"){
  x <- methods::as(0:1, class)
  rep(rep(x, each=2^(m-j)), length.out=2^m)
}

Hmat <- function(m, class="integer"){
  t(sapply(1:m, function(j) Hrow(j,m)))
}

H2mat <- function(m=3){
  H <- Hmat(m)
  I <- data.frame(j=rep(1:m, times=m:1-1), k=unlist(sapply(1:(m-1), function(j) (j+1):m)))
  t(apply(I, 1, function(x) H[x[1], ]*H[x[2], ]))
}

mat2vec <- function(M){
  m <- ncol(M)
  I <- data.frame(j=rep(1:m, times=m:1-1), k=unlist(sapply(1:(m-1), function(j) (j+1):m)))
  apply(I, 1, function(x) M[x[1], x[2]])
}

count_cells <- function(data){
  tab <- table(apply(data, 1, paste0, collapse=""))
  names(tab) <- bin2int(names(tab), m=ncol(data))
  tab
}

all_counts <- function(m, counts, default=0){
  sapply(as.character(1:2^m-1), function(x) ifelse(x %in% names(counts), counts[x], default))
}

bin2int <- function(b, m){
  sapply(strsplit(b, ""), function(x) sum(as.integer(x) * 2^(m:1-1)))
}

update_gamma <- function(data, prior=2/2^(ncol(data)), control=NULL, ...){
  all_counts(ncol(data), count_cells(data)) + prior
}


get_mode_mbeta <- function(vars, mode=c("auto", "full", "reduced"), ...){
  mode <- match.arg(mode)
  if(mode=="reduced" & vars==2){
    message("SIMPle: Using full parametrization for bivariate case.")
    mode <- "full"
  }
  if(mode=="full" & vars>10){
    warning("SIMPle: Full mbeta parametrization not recommended for dimensions > 10!")
  }
  if(mode=="auto"){
    mode <- ifelse(vars>10, "reduced", "full")
  }
  return(mode)
}




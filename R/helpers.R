

check_adm <- function(lower, upper, central=NULL){
  stopifnot(length(lower) == length(upper))
  if(!is.null(central)){
    stopifnot(length(lower) == length(central))
    return(all(lower <= central) & all(upper >= central))
  }
  return(all(lower <= upper))
}

Dir2mBeta <- function(x){
  if(!is.matrix(x)){x <- matrix(x, nrow=1)}
  m <- log(ncol(x), 2); stopifnot(m %% 1 == 0)
  x %*% t(Hmat(m))
  #sapply(1:m, function(j) rowSums(x[, select_cells(j, m), drop=F]))
}

select_cells <- function(j, m){
  stopifnot(j %in% 1:m)
  rep(rep(c(F,T), each=2^(m-j)), length.out=2^m)
}


Cmat <- function(k=1:2^m, m=3){
  C <- matrix(0, nrow=length(k), ncol=2^m)
  C[cbind(1:length(k), k)] <- 1
  return(C)
}


split_data <- function(data, by=rep(1, nrow(data)), names=unique(by), warn=10){
  stopifnot(all.equal(nrow(data), length(by), length(names)))
  u <- unique(by); stopifnot(length(names) == length(u))
  if(length(u) > warn){warning("Data split into ", length(u), " subgroups. Intended?")}
  dl <- lapply(u, function(x){data[by==u, ]})
  names(dl) <- names
  return(dl)
}














Hrow <- function(j, m, class="integer"){ # TODO: same as select_cells !!!!!!!!??????????!!!!!!!
  x <- as(0:1, class)
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

Htmat <- function(m){
  Ht <- rbind(Hmat(m), H2mat(m), 1)
  rownames(Ht) <- c(paste0("a", 1:m),
                    paste0("a", apply(data.frame(j=rep(1:m, times=m:1-1),
                                                 k=unlist(sapply(1:(m-1), function(j) (j+1):m))),
                                      1, function(x) paste(x, collapse="."))),
                    "nu")
  return(Ht)
}

check_mean <- function(dims, mean, ...){
  if(is.null(mean)){mean <- 1/2}
  stopifnot(is.numeric(mean))
  stopifnot(length(mean) %in% c(1, dims))
  if(length(mean) == 1){mean <- rep(mean, dims)}
  return(mean)
}

#' @importFrom matrixcalc is.positive.definite
check_corr <- function(dims, corr, ...){
  if(is.null(corr)){corr <- 0}
  if(is.null(dim(corr)) & is.numeric(corr)){
    corr <- rho2corr(dims, corr)
  }
  stopifnot(nrow(corr) == dims & ncol(corr) == dims)
  stopifnot(matrixcalc::is.positive.definite(corr))
  return(corr)
}

rho2corr <- function(dims, rho){
  if(is.matrix(rho)){
    stopifnot(matrixcalc::is.positive.definite(rho))
    return(rho)
  }
  matrix(rho, dims, dims) + diag(rep(1-rho, dims))
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





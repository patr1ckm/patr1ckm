#' Sample rows or columns of a matrix
#'
#' @param x matrix or data frame
#' @param n number of rows or columns to select
#' @param scol T/F. sample columns? Defaults to 'T'
shead <- function(x,n=10,scol=T){
  srow <- sample(1:nrow(x),size=n,replace=F)
  if(scol){
    scol <- sample(1:ncol(x),size=n,replace=F)
  } else {
    scol <- 1:ncol(x)
  }
  x[srow,scol]
}


#' Print the structure of a sample of columns of a matrix or data frame
#'
#' @param x matrix or data frame
#' @param n number of columns to sample
sstr <- function(x,n=10,...){
  scol <- sample(1:ncol(x),size=n,replace=FALSE)
  str(x[,scol],...)
}


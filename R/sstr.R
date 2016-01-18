#' Print the structure of a sample of columns or list elements
#'
#' @param x matrix or data frame, otherwise converts to data.frame
#' @param n number of columns to sample
sstr <- function(x,n=10,...){
  if(!(class(x) == "matrix" | class(x) == "data.frame")){
    x <- as.data.frame(x)
  }
  scol <- sample(1:ncol(x),size=n,replace=FALSE)
  str(x[,scol],...)
}


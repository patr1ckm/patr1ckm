#' Print the structure of a sample of columns or list elements
#'
#' @param x list, matrix or data frame, otherwise converts to data.frame
#' @param n number of columns to sample
#' @param ... arguments passed to ...
sstr <- function(x,n=10,...){
  if(class(x) == "matrix" | class(x) == "data.frame"){
    n <- min(ncol(x),n)
    scol <- sample(1:ncol(x),size=n,replace=FALSE)
    str(x[,scol],...)
  } else if (class(x) == "list") {
    n <- min(length(x),n)
    sid <- sample(1:length(x),size=n,replace=FALSE)
    str(x[sid],...)
  } else {
    x <- as.data.frame(x)
    n <- min(ncol(x),n)
    scol <- sample(1:ncol(x),size=n,replace=FALSE)
    str(x[,scol],...)
  }
}


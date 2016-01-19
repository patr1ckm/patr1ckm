#' Compute 'str' on the first n list elements or columns
#'
#' @param x list, matrix or data.frame, otherwise converts to data.frame
#' @param n number of list elements or columns
#' @param ... arguments passed to ...
#' @export
hstr <- function(x,n=10,...){
  if(class(x) == "matrix" | class(x) == "data.frame"){
    n <- min(ncol(x),n)
    str(x[,1:n],...)
  } else if (class(x) == "list") {
    n <- min(length(x),n)
    str(x[1:n],...)
  } else {
    x <- as.data.frame(x)
    n <- min(ncol(x),n)
    str(x[,1:n],...)
  }
}

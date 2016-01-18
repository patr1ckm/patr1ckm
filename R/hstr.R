#' Compute 'str' on the first n elements or columns
#'
#' @param x vector, list, matrix or data.frame
#' @param n number of list elements or columns
#' @param ids vector of list element ids or column ids (default 1:n)
hstr <- function(x,n=10,ids=NULL,...){
  if(!(class(x) == "matrix" | class(x) == "data.frame")){
    x <- as.data.frame(x)
  }
  if(is.null(cols)) {
    cols = 1:min(ncol(x), n)
  }
  str(x[,cols],...)
}

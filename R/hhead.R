#' Return a small number of rows and columns
#'
#' @param x matrix or data frame, otherwise converts to a matrix
#' @param n number of rows and columns to return
#' @param number of columns to return

hh <- function(x,n=10,ncols=n){
  if(!(class(x) == "matrix" | class(x) == "data.frame")){
    x <- as.matrix(x)
  }
  n <- min(nrow(x),n)
  cols <- 1:min(ncol(x),ncols)
  x[1:n,cols]
}




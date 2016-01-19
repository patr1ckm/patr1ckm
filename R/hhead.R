#' Return a small number of rows and columns
#'
#' @param x matrix or data frame, otherwise converts to a data.frame
#' @param nrow number of rows to return
#' @param ncol number of columns to return (default: nrow))
#' @export
hh <- function(x,nrows=10,ncols=nrows){
  if(!(class(x) == "matrix" | class(x) == "data.frame")){
    x <- as.data.frame(x)
  }
  nrows <- min(nrow(x),nrows)
  ncols <- min(ncol(x),ncols)
  x[1:nrows,1:ncols]
}




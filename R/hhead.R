#' Return a small number of rows and columns
#'
#' @param x list, matrix, or data.frame
#' @param nrow number of rows to return
#' @param ncol number of columns to return (default: nrow))
#' @export
hh <- function(x,nrows=10,ncols=nrows){
  if(class(x)=="list"){
    nrows <- min(length(x),nrows)
    return(lapply(x[1:nrows],head,n=nrows))
  }
  if(!(class(x) == "matrix" | class(x) == "data.frame")){
    x <- as.data.frame(x)
  }
  nrows <- min(nrow(x),nrows)
  ncols <- min(ncol(x),ncols)
  x[1:nrows,1:ncols]
}




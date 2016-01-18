#' Return a small non-overlapping number of rows and columns at beginning and end of data frame
#'
#' @param x matrix or data frame, otherwise converts to a data.frame
#' @param nrow number of rows to return
#' @param ncol number of columns to return (default: nrow))
ht <- function(x,nrows=10,ncols=nrows){
  if(!(class(x) == "matrix" | class(x) == "data.frame")){
    x <- as.data.frame(x)
  }
  nrows <- min(nrow(x),nrows)
  ncols <- min(ncol(x),ncols)

  headrow <- 1:nrows
  headcol <- 1:ncols
  tailrow <- nrows:nrow(x)
  tailcol <- ncols:ncol(x)
  rows <- unique(headrow,tailrow)
  cols <- unique(headcol,headrow)

  x[rows,cols]
}

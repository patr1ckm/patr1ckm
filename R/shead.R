#' Sample rows and columns of a matrix
#'
#' @param x matrix or data frame, otherwise converts to a matrix
#' @param nrow number of rows to sample
#' @param ncol number of columns to sample (defaults to nrow)
#' @param rows vector of row ids. If specified, rows are selected by row id and not sampled
#' @param cols vector of column ids. If specified, columns are selected by column id and not sampled

sh <- function(x,nrow=10,ncol=nrow,rows=NULL,cols=NULL){
  if(!(class(x) == "matrix" | class(x) == "data.frame")){
    x <- as.matrix(x)
  }
  if(is.null(rows)){
    rows <- sample(1:nrow(x),size=min(nrow(x),nrow),replace=F)
  }
  if(is.null(cols)){
    cols <- sample(1:ncol(x),size=min(ncol(x),ncol),replace=F)
  }
  x[rows,cols]
}


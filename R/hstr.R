#' Compute 'str' on the first n columns
#'
#' @param x matrix or data frame
#' @param n number of columns
#' @param cols vector of column ids (default 1:n)
hstr <- function(x,n=10,cols=1:n,...){
  str(x[,cols],...)
}

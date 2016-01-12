#' Return a small number of rows and columns
#'
#' @param x matrix or data frame
#' @param n number of rows and columns to return
#' @param cols vector of column ids (default 1:n)
hh <- function(x,n=10,cols=1:n){x[1:n,cols]}





#' Apply is.na to elements in margin of array or matrix
#'
#' @param x array, matrix, or data.frame
#' @param mar margin: 1=rows, 2=columns, etc.
#' @export
applyna <- function(x,mar=2){ apply(x,mar,function(v){is.na(v)})}

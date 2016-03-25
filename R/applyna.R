
#' Apply is.na to vector in margin of array or matrix
#'
#' @param x array, matrix, or data.frame
#' @param mar margin: 1=rows, 2=columns, etc.
#' @param FUN function to be applied to \code{is.na(vector)}, 
#' where \code{vector} is the vector of \code{x} implied by \code{mar}.
#'  Default is \code{I}, returning a logical vector.
#' @export
#' @examples
#' x <- matrix(5,5,5)
#' x[2,3] <- NA
#' applyna(x,2)
#' applyna(x,2,mean)
applyna <- function(x,mar=2,FUN=I){ apply(x,mar,function(v){FUN(is.na(v))})}

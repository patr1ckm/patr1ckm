
#' Return the ith element of a nested list
#' 
#' @param x (nested) list
#' @param i index of the element to return
#' @export
le <- function(x,i=1){lapply(x,function(el){el[[i]]})}

#' Return the ith element of a nested list, and unlist
#' 
#' @param x (nested) list
#' @param i index of the element to return
#' @export
ule <- function(x,i=1){unlist(le(x,i))}

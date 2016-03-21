#' Convert vector to symmetric matrix
#'
#' Reverse operation of \code{upper.tri} or \code{lower.tri}. Given a vector of 
#' upper or lower triangular elements of a matrix 
#' (optionally including the diagonal elements), returns the
#' corresponding symmetric matrix. The elements of the vector can be arranged by
#' column (the default) or by row.
#'
#' @param x vector containing upper or lower triangular elements of a symmetric matrix
#' @param If NULL, x contains diagonal elements. Otherwise a value or vector of the appropriate length to be placed on the diagonal.
#' @param lower x is from the lower triangle (default = \code{TRUE})
#' @param byrow the elements in x are ordered row-wise (default = \code{FALSE})
#' @return Symmetric matrix
#' @details Note that if x is a vector of the lower triagular elements given by column, 
#' this is equivalent to the upper triangular elements given by row. Similarly, if 
#' x is a vector of the lower triangular elements given by row, this is equivalent
#' to the upper triangular elements given by column. 
#' @examples 
#' x <- c(1,1,1,2,2,3)
#' check <- matrix(c(1,1,1,1,1,1,2,2,1,2,1,3,1,2,3,1),4,4) 
#' identical(vec2sym(x,diag=1,lower=T,byrow=F), check)
#' x <- c(1,1,1,2,2,3)
#' check <- matrix(c(1,1,1,1,2,2,1,2,3),3,3)
#' identical(vec2sym(x,lower=T,byrow=F),check)
#' @export
vec2sym <- function(x,diagonal=NULL,lower=TRUE,byrow=FALSE){
  if(is.null(diagonal)){
    ## Assume that x contains the diagonal elements as well
    p <- (sqrt(1 + 8 * length(x)) - 1)/2
    S <- diag(p)
    if((!lower & byrow) | (lower & !byrow)){
      S[lower.tri(S,diag=T)] <- x
      S[which(lower.tri(t(S),diag=T),arr.ind=T)[,c(2,1)]] <- x
    }
    if((lower & byrow) | (!lower & !byrow)) {
      S[upper.tri(S,diag=T)] <- x
      S[which(upper.tri(t(S),diag=T),arr.ind=T)[,c(2,1)]] <- x
    }
  } else{
    ## diagonal elements are given by 'diagonal'
    p <- (sqrt(1 + 8 * length(x)) + 1)/2
    S <- diag(p)
    if((!lower & byrow) | (lower & !byrow)){
      S[lower.tri(S)] <- x
      S[which(lower.tri(t(S)),arr.ind=T)[,c(2,1)]] <- x
    }
    if((lower & byrow) | (!lower & !byrow)) {
      S[upper.tri(S)] <- x
      S[which(upper.tri(t(S)),arr.ind=T)[,c(2,1)]] <- x
    }
    diag(S) <- diagonal
  }

  return(S)
}

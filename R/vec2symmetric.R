#' convert vector to symmetric matrix
#'
#' Given a vector of upper or lower triangular elements of a matrix, returns the
#' corresponding symmetric matrix. The elements of the vector can be arranged by
#' column (the default) or by row.
#'
#' @param x vector containing upper or lower triangular elements of a symmetric matrix
#' @param diagonal value or vector of elements to place on the diagonal
#' @param lower x is from the lower triangle (default = \code{TRUE})
#' @param byrow the elements in x are ordered row-wise (default = \code{FALSE})
#' @export
vec2sym <- function(x,diagonal=NULL,lower=TRUE,byrow=FALSE){
  p <- (sqrt(1 + 8 * length(x)) + 1)/2 # solution to quadratic formula
  S <- diag(p)
  if((!lower & byrow) | (lower & !byrow)){
    S[lower.tri(S)] <- x
    S[which(lower.tri(t(S)),arr.ind=T)[,c(2,1)]] <- x
  }
  if((lower & byrow) | (!lower & !byrow)) {
    S[upper.tri(S)] <- x
    S[which(upper.tri(t(S)),arr.ind=T)[,c(2,1)]] <- x
  }
  if(!is.null(diagonal)){
    diag(S) <- diagonal
  }
  return(S)
}

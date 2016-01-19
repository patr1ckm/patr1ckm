#' checks if \code{x} is \code{NA}, \code{NaN}, or \code{Inf}.
#'
#' Returns \code{TRUE} if \code{x} is \code{NA}, \code{NaN}, or \code{Inf}.
#' @param x value
#' @export
is.special <- function(x){
  if (is.numeric(x)) !is.finite(x) else is.na(x)
}

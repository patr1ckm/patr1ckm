#' Assign default args of a function into global environment
#'
#' useful for interactive debugging of functions, call this then step through the function.
#'
#' @param f function name
#' @return Called for the side effect
#' @export
putArgs <- function(f) {
  mapply(FUN=assign,x=names(formals(f)),value=formals(f),MoreArgs=list(env=.GlobalEnv))
}

f <- function(a=1,b=2){a+b}

#' Assign default args of a function into global environment
#'
#' useful for interactive debugging of functions, call this then step through the function.
#'
#' @param f function name
#' @param env environment, default is Global
#' @return Called for the side effect
#' @export
#'
put.args <- function(f,env=.GlobalEnv) {
  list2env(as.list(formals(f)),env=env)
  #mapply(FUN=assign,x=names(formals(f)),value=formals(f),MoreArgs=list(env=.GlobalEnv))
}

#f <- function(a=1,b=2){a+b}

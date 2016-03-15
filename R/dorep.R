#' Evaluate a function repeatedly over arbitrary arguments
#'
#' This idiom is really useful to carry out simulations, which are essentially
#' repeated evaluations of a function over a grid of parameter values.
#'
#' @param f function to be evaluated
#' @param reps the number of times the function should be evaluated
#' @param ... Arguments passed to f
#' @export
#' @examples
#' conds <- expand.grid(a=1:3,b=4:5)
#' conds.ls <- split(conds, 1:nrow(conds))
#' names(conds.ls) <- NULL
#' do.one <- function(a=1,b=2){a+b}
#' lapply(conds.ls, do.cond, FUN=do.one, reps=5)
do.rep <- function(f,reps,...){ do.call(rbind, lapply(1:reps,function(r, f, ...){ do.call(f,...)}, f=f, ...))}

# conds <- expand.grid(a=1:3,b=4:5)
# conds.ls <- split(conds, 1:nrow(conds))
# names(conds.ls) <- NULL
# do.one <- function(a=1,b=2){a+b}
# lapply(conds.ls, FUN=do.cond,f=do.one,reps=7)
#
#
# conds <- expand.grid(a=1:3,b=4:5)
# conds.ls <- split(conds, 1:nrow(conds))
# names(conds.ls) <- NULL
# do.one <- function(a=1,b=2){a+b}
# do.cond <- function(reps,...){ do.call(rbind, lapply(1:reps,function(r){ do.call(do.one,...)}))}
# lapply(conds.ls,do.cond,reps=7)

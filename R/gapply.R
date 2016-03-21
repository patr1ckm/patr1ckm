#' Repeatedly apply a function over a grid of values in parallel
#'
#' gapply (grid apply) applies a function to a grid of it's parameters in parallel, optionally for a given number of replications
#'
#' @param f function to be evaluated. The function must return a (named) value or (named) vector of values.
#' @param reps times the function should be evaluated
#' @param mc.cores attempts to split function evaluations over given number of cores
#' @param ... named arguments to \code{f} in the form \code{key=c(value1,value2, ...)} etc. 
#' A grid of parameter values will be generated from values given to each named argument, as \code{expand.grid(...)}
#' @return Returns results as a \code{data.frame} in long form with first column \code{param.id}, the row of \code{expand.grid(...)}.
#' This is followed by columns of \code{expand.grid(...)}, followed by \code{rep} giving the replication number. 
#' Finally \code{key} gives the name of the return value of \code{f},
#' and \code{value} gives the value. if results from \code{f} are named. 
#' @details Note that the function application (not replications) are distributed in parallel, will not work in Windows.
#' @examples
#' do.one <- function(a=1,b=2){c(sum=a+b,sub=a-b)}
#' gapply(do.one,reps=5, a=1:4,b=2:3)
#' @export
#' @importFrom tidyr gather
#' @importFrom parallel mclapply
gapply <- function(f, reps=1, mc.cores=1, ...){
  param.grid <- expand.grid(...)
  param.ls <- split(param.grid, 1:nrow(param.grid))
  names(param.ls) <- NULL
  res <- parallel::mclapply(param.ls, do.rep, f=f, reps=reps,mc.cores=mc.cores)
  wide <- as.data.frame(cbind(param.id=rep(1:nrow(param.grid),each=reps),
                              rep=rep(1:nrow(param.grid), times=reps),
                              do.call(rbind,res)))
  long <- tidyr::gather(wide,key,value,-(1:2))
  param.grid.id <- data.frame(param.grid, param.id=1:nrow(param.grid))
  long.param <- merge(param.grid.id,long)
  return(long.param)
}


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
do.rep <- function(f,reps,...){
  res <- do.call(rbind, lapply(1:reps,function(r, f, ...){ do.call(f,...)}, f=f, ...))
  as.data.frame(res) # need this to get automatic reasonable naming of columns as default
}



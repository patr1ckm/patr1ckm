#' Repeatedly apply a function over a grid of values in parallel
#'
#' gapply (grid apply) applies a function to a grid of it's parameters, optionally for a given number of replications
#'
#' @param f function to be evaluated, should return a value or vector for best results
#' @param reps times the function should be evaluated
#' @param mc.cores attempts to split function evaluations over given number of cores
#' @param ... arguments to \code{f} in the form \code{a=1:3, b=2:4}, etc. A grid of parameter values will be generated from values given to each argument
#' @return Returns results as a data.frame in long form with first column \code{param.id}. This is followed
#' by column \code{res} or column(s) \code{res.*} if results from \code{f} are named. The final columns contain the
#' values of parameters.
#' @details Note that the function application (not replications) are distributed in parallel, will not work in Windows.
#' @examples
#' do.one <- function(a=1,b=2){data.frame(sum=a+b,sub=a-b)}
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



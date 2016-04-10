#' Repeatedly apply a function over a grid of values in parallel
#'
#' gapply (grid apply) applies a function to a grid of it's parameters in parallel, optionally for a given number of replications
#'
#' @param f function to be evaluated. The function must return a (named) value or (named) vector of values.
#' @param ... named arguments to \code{f} in the form \code{key=c(value1,value2, ...)} etc. 
#' @param reps times the function should be evaluated
#' @param mc.cores attempts to split function evaluations over given number of cores
#' @param verbose If \code{1} (default), prints a \code{.} with every completed condition. 
#' If \code{2}, prints the arguments corresponding to the completed condition. 
#' If \code{3}, prints the arguments and results of the completed condition.
#' A grid of parameter values will be generated from values given to each named argument, as \code{expand.grid(...)}
#' @return Returns results as a \code{data.frame} in long form with the following columns:
#' \item{\code{param.id}}{the row of \code{expand.grid(...)}}
#' \item{...}{Columns corresponding to grid of parameters given in \code{expand.grid(...)}}
#' \item{\code{rep}}{giving the replication number}
#' \item{\code{key}}{giving the name(s) of the return value(s) of \code{f}}
#' \item{\code{value}}{gives the value}
#' @details If the values returned by \code{f} are not named, they will be named according to the rules of \code{as.data.frame}, typically \code{V1, V2, ...}. 
#' Note that the function application to each combination of meta-parameters (not replications) are distributed in parallel, will not work in Windows. 
#' @examples
#' do.one <- function(a=1,b=2){c(sum=a+b,sub=a-b)}
#' gapply(do.one,reps=5, a=1:4,b=2:3)
#' @export
#' @importFrom tidyr gather
#' @importFrom parallel mclapply
#' @importFrom dplyr rbind_all
gapply <- function(f, ..., .reps=1, .mc.cores=1, .verbose=1, .eval=T){
  param.grid <- expand.grid(...)
  param.ls <- split(param.grid, 1:nrow(param.grid))
  names(param.ls) <- NULL
  start <- proc.time()
  res <- parallel::mclapply(param.ls, do.rep, f=f, 
                            .reps=.reps, mc.cores=.mc.cores, .verbose=.verbose, 
                            .eval=.eval, .rep.cores=1)
  end <- proc.time()
  full <- do.call(rbind, res)
  wide <- as.data.frame(cbind(param.id=rep(1:nrow(param.grid),each=.reps),
                              rep=rep(1:.reps, times=nrow(param.grid)),
                              full))
  long <- tidyr::gather(wide,key,value,-(1:2))
  param.grid.id <- data.frame(param.grid, param.id=1:nrow(param.grid))
  long.param <- merge(param.grid.id,long)
  class(long.param) <- c("gapply", class(long.param))
  attr(long.param, "time") <- end-start
  attr(long.param, "arg.names") <- colnames(param.grid)
  attr(long.param, "f") <- f
  attr(long.param, "grid") <- param.grid
  return(long.param)
}

#' Evaluate a function repeatedly over arbitrary arguments
#'
#' This idiom is really useful to carry out simulations, which are essentially
#' repeated evaluations of a function over a grid of parameter values.
#'
#' @param f function to be evaluated
#' @param ... Arguments passed to f
#' @param reps the number of times the function should be evaluated
#' @param verbose If \code{1} (default), prints a \code{.} with every completed condition. 
#' If \code{2}, prints the arguments corresponding to the completed condition. 
#' If \code{3}, prints the arguments and results of the completed condition.
#' @export
#' @importFrom parallel mclapply
#' @importFrom dplyr rbind_all as.tbl
#' @details If verbose = 
#' @examples
#' conds <- expand.grid(a=1:3,b=4:5)
#' conds.ls <- split(conds, 1:nrow(conds))
#' names(conds.ls) <- NULL
#' do.one <- function(a=1,b=2){a+b}
#' lapply(conds.ls, do.cond, FUN=do.one, reps=5)
do.rep <- function(f,..., .reps,.verbose=1,.rep.cores=1, .eval=T){
  if(.verbose %in% c(2,3)){cat(paste(names(...),"=", ...),fill=T)}
  if(.eval){
    res.l <- parallel::mclapply(1:(.reps),function(r, f, ...){ 
      (try(do.call(f,...)))}, f=f, ..., mc.cores=.rep.cores)
  } else {
    nothing <- function(...){c(NA)}
    res.l <- lapply(1:.reps, function(r, ...) do.call(nothing, ...), ...)
  }
  res <- as.data.frame(do.call(rbind, res.l))
  if(.verbose==1){cat(".")}
  if(.verbose == 3) { print(head(res))}
  if(.verbose > 0) { cat("", fill=T) }
  res # need this to get automatic reasonable naming of columns as default
}



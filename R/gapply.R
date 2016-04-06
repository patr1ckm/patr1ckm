#' Repeatedly apply a function over a grid of values in parallel
#'
#' gapply (grid apply) applies a function to a grid of it's parameters in parallel, optionally for a given number of replications
#'
#' @param f function to be evaluated. The function must return a (named) value or (named) vector of values.
#' @param reps times the function should be evaluated
#' @param mc.cores attempts to split function evaluations over given number of cores
#' @param verbose If \code{1} (default), prints a \code{.} with every completed condition. 
#' If \code{2}, prints the arguments corresponding to the completed condition. 
#' If \code{3}, prints the arguments and results of the completed condition.
#' @param ... named arguments to \code{f} in the form \code{key=c(value1,value2, ...)} etc. 
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
gapply <- function(f, reps=1, mc.cores=1, verbose=1, ...){
  param.grid <- expand.grid(...)
  param.ls <- split(param.grid, 1:nrow(param.grid))
  names(param.ls) <- NULL
  start <- proc.time()
  res <- parallel::mclapply(param.ls, do.rep, f=f, reps=reps,mc.cores=mc.cores, verbose=verbose)
  end <- proc.time()
  wide <- as.data.frame(cbind(param.id=rep(1:nrow(param.grid),each=reps),
                              rep=rep(1:reps, times=nrow(param.grid)),
                              do.call(rbind,res)))
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
#' @param reps the number of times the function should be evaluated
#' @param verbose If \code{1} (default), prints a \code{.} with every completed condition. 
#' If \code{2}, prints the arguments corresponding to the completed condition. 
#' If \code{3}, prints the arguments and results of the completed condition.
#' @param ... Arguments passed to f
#' @export
#' @details If verbose = 
#' @examples
#' conds <- expand.grid(a=1:3,b=4:5)
#' conds.ls <- split(conds, 1:nrow(conds))
#' names(conds.ls) <- NULL
#' do.one <- function(a=1,b=2){a+b}
#' lapply(conds.ls, do.cond, FUN=do.one, reps=5)
do.rep <- function(f,reps,verbose=1,...){
  if(verbose %in% c(2,3)){cat(paste(names(...),"=", ...),fill=T)}
  res <- do.call(rbind, lapply(1:reps,function(r, f, ...){ do.call(f,...)}, f=f, ...))
  if(verbose==1){cat(".",fill=T)}
  if(verbose == 3) { cat(paste(names(res), "=", res),fill=T)}
  as.data.frame(res) # need this to get automatic reasonable naming of columns as default
}

#' \code{summary} method for class \code{"gapply"}
#' @param object gapply result object
#' @param nreps number of reps to scale to
#' @return Prints the means over all reps for each condition and returns it invisibly. Also prints the estimated time to scale up to x reps
#' @importFrom dplyr group_by_ summarize
#' @importFrom tidyr %>%
#' @export
summary.gapply <- function(object, nreps=NULL){
  ns <- c(attr(object, 'arg.names'),"key")
  means <- object %>% dplyr::group_by_(.dots=ns) %>% dplyr::summarize(mean(value), sd(value))
  print(means)
  cat("",fill=T)
  grid <- attr(object, "grid")
  cat("Number of conditions: ", nrow(grid), fill=T)
  print(head(grid))
  cat("",fill=T)
  if(!is.null(attr(out,"time"))){
    cat("Estimated time for x reps:", fill=T)
    cat("Reps \t Time", fill=T)
    o <- estimate.time(object, nreps=nreps)
    for(i in 1:nrow(o)){ cat(paste0(o[i,],"\t"),fill=T)}
  }
  invisible(means)
}

#' Estimate time for a given number of reps
#' @param object gapply object
#' @param nreps number of reps to scale to
#' @export
estimate.time <- function(object, nreps=NULL){
  if(is.null(nreps)){ nreps <- c(50,100,500,1000,5000,10000)}
  max.reps <- max(object$rep)
  time.per.rep <- attr(object,"time")[3]/max.reps
  times <- lapply(time.per.rep*nreps,FUN=nicetime)
  o <- cbind(reps=nreps,times=times)
  return(o)  
}

## From package astro
nicetime <- function (seconds) {
  lapseconds = round(seconds)
  seconds = lapseconds%%60
  minutes = ((lapseconds - seconds)/60)%%60
  hours = ((lapseconds - minutes * 60 - seconds)/3600)%%24
  days = ((lapseconds - hours * 3600 - minutes * 60 - seconds)/86400)
  lapline = {
  }
  if (days != 0) {
    if (days == 1) {
      lapline = paste(days, "d, ", sep = "")
    }
    else {
      lapline = paste(days, "d, ", sep = "")
    }
  }
  if (hours != 0 | days != 0) {
    if (hours == 1) {
      lapline = paste(lapline, hours, "h, ", sep = "")
    }
    else {
      lapline = paste(lapline, hours, "h, ", sep = "")
    }
  }
  if (minutes != 0 | hours != 0 | days != 0) {
    if (minutes == 1) {
      lapline = paste(lapline, minutes, "m, ", sep = "")
    }
    else {
      lapline = paste(lapline, minutes, "m, ", sep = "")
    }
  }
  if (seconds == 1) {
    lapline = paste(lapline, seconds, "s", sep = "")
  }
  else {
    lapline = paste(lapline, seconds, "s", sep = "")
  }
  return(lapline)
}

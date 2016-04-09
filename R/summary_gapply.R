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
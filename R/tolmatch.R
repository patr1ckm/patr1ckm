#' Match with tolerance
#' @export
tol.match <- function(x, tab, tol=.Machine$double.eps){
  res <- rep(NA, length(x))
  for(i in 1:length(tab)){
    res[abs(x - tab[i]) <= tol] <- i
  }
  return(res)
}
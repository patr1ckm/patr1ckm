
#' Compute all pairwise tables for data
#'
#' @param x matrix or data.frame
#' @return list of all pairwise tables
#' @export
pw.tables <- function(x){
  idx <- which(lower.tri(diag(ncol(x)),diag=F),arr.ind=T)
  tab.ls <- list()
  for(i in 1:nrow(idx)){
    tab.ls[[i]] <- table(x[,idx[i,]])
  }
  return(tab.ls)
}

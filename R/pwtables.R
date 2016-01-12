
## Compute all pairwise tables for data
pw.tables <- function(data){
  idx <- which(lower.tri(diag(ncol(data)),diag=F),arr.ind=T)
  tab.ls <- list()
  for(i in 1:nrow(idx)){
    tab.ls[[i]] <- table(data[,idx[i,]])
  }
  return(tab.ls)
}

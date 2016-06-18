
# hopefully useful data analysis helper functions.

# Check a correlation matrix for highly correlated variables at a given cutoff.
#' @export
check.cor <- function(cormat=NULL,d=NULL,cutoff=.8){
  if(is.null(cormat)) cormat <- cor(d,use="pairwise")
  diag(cormat) <- 0
  ids <- which(abs(cormat) > .8,arr=TRUE)
  namr <- colnames(cormat)[ids[,1]]
  namc <- colnames(cormat)[ids[,2]]
  return(data.frame(ids,var.row=namr,var.col=namc,cor=cormat[ids]))
}
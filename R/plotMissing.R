#' plots missing values
#'
#' @param d data
#' @param main title
#' @param varnames variable names
#' @param minss minimum sample size of pattern to plot
#' @param topvars top number of variables
plotMissing <- function(d,main="",varnames=NULL,minss=50,transpose=FALSE,...) {
  require(rsem)
  require(corrplot)
  d <- d[!id.allmiss(d,1),]
  capture.output(pat <- rsem.pattern(d)$mispat, file='/dev/null')
  #require(grid)
  if(is.null(varnames)) {varnames <- c("n","nvar",paste("X",1:(ncol(pat)-2),sep=""))}
  rownames(pat) <- 1:nrow(pat)
  vsort <- pat[order(pat[,2],pat[,1],decreasing=TRUE),]
  #vsort.filterss <- vsort[vsort[,1]>minss,]
  ssort <- pat[order(pat[,1],decreasing=TRUE),]
  ssort <- ssort[ssort[,1] > minss,]
  #colnames(rev) <- rev[1,]
  #rownames(rev) <- varnames
  #rev <- t(vsort)
  rownames(ssort) <- ssort[,1] # replace rownames with sample sizes
  ssort <- ssort[,-c(1:2)] # drop n, nvar
  colnames(ssort) <- varnames
  #rev <- t(ssort)
  if(transpose){
    ssort <- t(ssort)
  }

  #corrplot(rev[-c(1:2),],method = "square",is.corr = FALSE,addshade = F)
  corrplot(ssort,method = "square",is.corr = FALSE,addshade = FALSE,
           col=c("white","gray"),outline=FALSE,addgrid.col="gray",cl.pos="n",...)

}

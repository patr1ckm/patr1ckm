#' Quickly reinstall the already loaded package from github
#' 
#' @export
reup <- function(x){
  detach("package:patr1ckm")
  remove.packages("patr1ckm")
  #system("R CMD REMOVE patr1ckm")
  devtools::install_github("patr1ckm/patr1ckm")
  library(patr1ckm)
}

#' Push to github
#' @param m commit message
#' @export
push <- function(m, f=NULL){
  if(!is.null(f)){ 
    cmd <- paste0("git add ", f)
    system(cmd)
  }
  cmd <- "git add -u"
  system(cmd)
  cmd <- paste0("git commit -m '", m, "'")
  system(cmd)
  cmd <- paste0("git push origin master")
  system(cmd)
}

## testing
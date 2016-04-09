#' Quickly reinstall the already loaded package from github
#' 
#' @export
reup <- function(x){
  detach("package:patr1ckm")
  remove.packages("patr1ckm")
  devtools::install_github("patr1ckm/patr1ckm")
  library(patr1ckm)
}

#' Push to github
#' 
#' @export
push <- function(m, f=NULL){
  if(!is.null(f)){ 
    cmd <- paste0("git add R/", f)
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
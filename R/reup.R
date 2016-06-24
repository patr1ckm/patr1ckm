#' Quickly reinstall the already loaded package from github
#' 
#' @export
reup <- function(x, user="patr1ckm"){
  detach(paste0("package:", x), character.only = T, unload = T)
  remove.packages(x)
  #system("R CMD REMOVE patr1ckm")
  devtools::install_github(paste0(user, "/", x))
  q(save="no")
}

#' Push changes to tracked files to master branch on github
#' @param m commit message
#' @export
push <- function(msg, f=NULL){
  if(!is.null(f)){ 
    cmd <- paste0("git add ", f)
    system(cmd)
  }
  cmd <- "git add -u"
  system(cmd)
  cmd <- paste0("git commit -m '", msg, "'")
  system(cmd)
  cmd <- paste0("git push origin master")
  system(cmd)
}

#' add and commit changes
#' @param m commit message
#' @export
commit <- function(m, f=NULL){
  if(!is.null(f)){ 
    cmd <- paste0("git add ", f)
    system(cmd)
  }
  cmd <- "git add -u"
  system(cmd)
  cmd <- paste0("git commit -m '", m, "'")
  system(cmd)
}


## testing
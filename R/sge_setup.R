## Interactive setting up of simulation from gapply test object

#' setup sge simulation
#' @param dir directory name relative to the current working directory, ends in '/'
#' @export
setup <- function(object, dir="",  .reps=1, .chunks = 1, .mc.cores=1, .verbose=1, .script.name="doone.R"){
  param.grid <- attr(object,"grid")
  chunk.grid <- param.grid[rep(1:nrow(param.grid), each=.chunks),,drop=F]
  chunk.grid$chunk <- rep(1:.chunks, each=nrow(param.grid))
  f <- attr(object,"f")
  
  cmd <- paste0("mkdir -p ", dir, "results") 
  mysys(cmd)
  cmd <- paste0("mkdir -p ", dir, "SGE_Output")
  mysys(cmd)
  
  write.submit(dir, script.name=.script.name, mc.cores=.mc.cores, tasks=nrow(param.grid))

  param.grid <- chunk.grid
  save(param.grid, file=paste0(dir, "param_grid.Rdata"))
  
  write.do.one(f=f, dir=dir, reps=.reps, mc.cores=.mc.cores, verbose=.verbose, script.name=.script.name)
}


qst <- function(){
  mysys("qst")
}

mysys <- function(cmd){
  cat(cmd,fill=T)
  system(cmd)
}

write.submit <- function(dir="", script.name="doone.R", mc.cores=1, tasks=1){
  cmd <- paste0("touch ", dir, "submit")
  mysys(cmd)
  temp <- paste0("#!/bin/bash
#$ -M patr1ckm.crc@gmail.com     # Email address for job notification
#$ -m a          # Send mail when job begins, ends and aborts
#$ -pe smp ",mc.cores,"     # environment and legal core size
#$ -q *@@daccss  # Specify queue
#$ -N patr1ckm   # Specify job name
#$ -t 1:", tasks, "        # number of rows in param.grid
#$ -o SGE_Output

Rscript ", script.name, " $SGE_TASK_ID")
  cat(temp,file=paste0(dir, "submit"))
}

write.do.one <- function(f, dir, reps=1, mc.cores=1, verbose=1, script.name="doone.R"){
  fstr <- paste0("f <- ", paste0(deparse(eval(f), control="all"),collapse="\n"))
  temp <- paste0(fstr,"
  library(patr1ckm)
  args <- as.numeric(commandArgs(trailingOnly=TRUE))
  cond <- args[1]
  reps <- ", reps,"
  load('param_grid.Rdata')
  params <- param.grid[cond,]
  rep.id <- (reps*(params$chunk-1)+1):(reps*params$chunk)
  params$chunk <- NULL # because f doesn't take chunk usually
  res.l <- do.rep(f, as.list(params), .reps=reps, .rep.cores=", mc.cores, ", .verbose=", verbose," )
  dir <- paste0('results/cond_', cond,'/')
  system(paste0('mkdir -p ', dir))
  fn <- paste0(dir, 'cond_', cond,'_reps_',rep.id[1],'-', rep.id[reps],'.Rdata')
  save(res.l, file=fn)")
  
  cat(temp, file=paste0(dir, script.name))
}

#' Submit jobs to SGE
#' @export
submit <- function(dir=""){
  wd <- getwd()
  setwd(dir)
  cmd <- paste0("qsub submit")
  mysys(cmd)
  setwd(wd)
}

#' Collect completed results files
#' @export
#' @importFrom gtools mixedsort
#' @importFrom tidyr gather
collect <- function(dir=""){
  load(paste0(dir, "param_grid.Rdata"))
  
  rdir <- paste0(dir, "results/")
  conds.files <- paste0(rdir,list.files(rdir))
  res.l <- list()           # list of the results from each condition 
  err.l <- list()

  na.reps <- function(fn){
    if(file.exists(fn)){
      load(fn)
      return(res.l)
    } else return(list(NA))
  }
  
  for(i in 1:length(conds.files)){
    rep.files <- gtools::mixedsort(list.files(conds.files[[i]]))
    reps.list <- list()
    for(j in 1:length(rep.files)){
      fn <- paste0(conds.files[i], "/", rep.files[j])
      reps.list[[j]] <- na.reps(fn)
    }
    res.l[i] <- reps.list
  }
  .reps <- length(reps.list[[1]]) # Since this is from do.rep, will always be of length reps
  res.l <- unlist(res.l, recursive=FALSE) # should be conds*reps long
  
  param.grid$chunk <- NULL
  rep.grid <- param.grid[rep(1:nrow(param.grid),each=.reps), , drop=F]
  rep.grid$rep  <- rep(1:.reps, times=nrow(param.grid))
  
  err.id <- unlist(lapply(res.l, is.error))
  err.list <- res.l[err.id]
  value <- as.data.frame(do.call(rbind, res.l[!err.id])) # automatic naming of unnamed returns to V1,V2, etc
  
  wide <- cbind(rep.grid[!err.id, ], value)
  
  long <- tidyr::gather(wide,key,value,-(1:3))
  
  class(long) <- c("gapply", class(long))
  attr(long, "time") <- NULL
  attr(long, "arg.names") <- colnames(param.grid)
  attr(long, "f") <- NULL
  attr(long, "grid") <- param.grid
  attr(long, "err") <- lapply(err.list,as.character)
  
  return(long)
}

#' Cleans results
#' @param dir project directory name followed by 'slash'
#' @export
clean <- function(dir){
  rdir <- paste0(dir, "results/")
  if(file.exists(rdir)){
    cmd <- paste0("rm -rf ", rdir)
    mysys(cmd)
  }
}

#' sge
#' 
#' @export
sge <- function(dir="tmp/"){
  f <- function(x,y){
    Sys.sleep(.5)
    stopifnot(x < 5)
    x
  }
  out <- gapply(f, x=3:8, y=1:2, .eval=F)
  setup(out, dir)
  submit(dir)
}


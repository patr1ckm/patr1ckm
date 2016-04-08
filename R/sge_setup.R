## Interactive setting up of simulation from gapply test object

#' setup sge simulation
#' @param dir directory name relative to the current working directory, ends in '/'
#' @export
setup <- function(object, dir="",  nreps=1, chunk.size = nreps, mc.cores=1, verbose=1, script.name="doone.R"){
  param.grid <- attr(object,"grid")
  chunks <- ceiling(nreps/chunk.size)
  chunk.grid <- do.call(rbind,replicate(chunks, param.grid, simplify = F))
  param.grid <- data.frame(chunk.grid, chunk=rep(1:chunks, each=nrow(param.grid)))
  f <- attr(object,"f")
  cmd <- paste0("mkdir ", dir, "results") 
  mysys(cmd)
  cmd <- paste0("mkdir ", dir, "SGE_Output")
  mysys(cmd)
  sn <- paste0(dir, script.name)
  write.submit(dir, script.name=sn, mc.cores=mc.cores, tasks=nrow(param.grid))
  save(param.grid, file=paste0(dir, "param_grid.Rdata"))
  write.do.one(f=f,  nreps=nreps, chunk.size=chunk.size, mc.cores=mc.cores, verbose=verbose, script.name=sn)
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

write.do.one <- function(f, nreps=1, chunk.size=nreps, mc.cores=1, verbose=1, script.name="doone.R"){
  fstr <- paste0("f <- ", paste0(deparse(eval(f)),collapse=""))
  temp <- paste0(fstr,"
  args <- as.numeric(commandArgs(trailingOnly=TRUE))
  cond <- args[1]
  nReps <- ", nreps,"
  load('param_grid.Rdata')
  params <- param.grid[cond,]
  reps <- (nReps*(params$chunk-1)+1):(nReps*params$chunk)
  res <- do.rep(f,", nreps, ", rep.cores=", mc.cores, ", verbose=", verbose, ", as.list(params))
  dir <- paste0('results/cond_', cond,'/')
  system(paste0('mkdir ', dir))
  fn <- paste0(dir, 'cond_', cond,'_reps_',reps[1],'-', reps[length(reps)],'.Rdata')
  save(res, file=fn)")
  cat(temp, file=script.name)
}

#' Submit jobs to SGE
#' @export
submit <- function(dir=""){
  cmd <- paste0("qsub ", dir, "submit")
  mysys(cmd)
}

#' Collect completed results files
#' @export
collect <- function(dir=""){
  load(paste0(dir, "param_grid.Rdata"))
  rdir <- paste0(dir, "results/")
  conds.files <- paste0(rdir,list.files(rdir))
  res.list <- list()
  for(i in 1:length(conds.files)){
    rep.files <- list.files(conds.files[[i]])
    reps.list <- list()
    for(j in 1:length(rep.files)){
      load(paste0(conds.files[i], "/", rep.files[j]))
      reps.list[[j]] <- res
    }
    res.list[[i]] <- do.call(rbind, reps.list)
  }
  res <- do.call(rbind, res.list)
  reps <- nrow(reps.list[[1]])
  wide <- as.data.frame(cbind(param.id=rep(1:nrow(param.grid),each=reps),
                              rep=rep(1:reps, times=nrow(param.grid)),
                              res))
  long <- tidyr::gather(wide,key,value,-(1:2))
  param.grid.id <- data.frame(param.grid, param.id=1:nrow(param.grid))
  long.param <- merge(param.grid.id,long)
  class(long.param) <- c("gapply", class(long.param))
  #attr(long.param, "time") <- end-start
  attr(long.param, "arg.names") <- colnames(param.grid)
  #attr(long.param, "f") <- f
  attr(long.param, "grid") <- param.grid
  return(long.param)
}



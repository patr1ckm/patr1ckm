doCallWE <- function(f, argl, timer = mkTimer(gcFirst=FALSE))
{
  tim <- timer( res <- tryCatch.W.E( do.call(f, argl) )) # compute f(<argl>)
  is.err <- is(val <- res$value, "simpleError") # logical indicating an error
  list(value   = if(is.err) NULL else val, # value (or NULL in case of error)
       error   = if(is.err) val else NULL, # error (or NULL if okay)
       warning = res$warning, # warning (or NULL)
       time    = tim) # time
}

tryCatch.W.E <- function(expr){
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w.handler),
       warning = W)
}
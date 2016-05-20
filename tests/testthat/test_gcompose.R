
## Here is the setup
e <- function(){stop("this is an error")}
w <- function(){warning("this is a warning"); 1} # still returns a value

f <- function(y=1,deff=NULL,deff2=5){
  if(y == 1){ e()}
  if(y == 2){ w(); return(deff2)}
}
# should ignore the first argument right? assumed to be output of f, whatever it's name.
g1 <- function(x, arg1, defg=3){ 
  x + arg1
}
g2 <- function(x, arg2, defg2=3){
  x + arg2
}

## in ..., want to grid
args <- list(y=1:2, deff2=1:2, defg=3:4, defg2=4:5)
ng <- list(arg1=c(1,1), arg2=c(2,2))
g.args <- list(defg=3)
f.args <- args

## start with the basics and work up.

## Test fgWE

x <- patr1ckm:::fgWE(.f=f, .g=g1, f.args=le(f.args,1), g.args=c(le(ng,1),g.args))
expect_true(is.na(x))
expect_true(!is.null(attr(x, "err")))
x <- patr1ckm:::fgWE(.f=f, .g=g1, f.args=le(f.args,2), g.args=c(le(ng,1)[1],g.args))
expect_true(x==3)
expect_true(!is.null(attr(x, "warn")))
x <- patr1ckm:::fgWE(.f=f, .g=g2, f.args=le(f.args,2), g.args=c(le(ng,2)[2]))
expect_true(x==4)
expect_true(!is.null(attr(x,"warn")))

## Test the do.rep call of fgWE
x <- do.rep(f=patr1ckm:::fgWE, list(.f=f, .g=g1, f.args=le(f.args,2), g.args=c(le(ng,1)[1],g.args)), 
       .reps=1,.verbose=0, .rep.cores=1, .eval=T)
expect_true(unlist(x)==3)
expect_true(!is.null(attr(x[[1]], "warn")))

x <- do.rep(f=patr1ckm:::fgWE, list(.f=f, .g=g2, f.args=le(f.args,2), g.args=c(le(ng,1)[2])), 
            .reps=1,.verbose=0, .rep.cores=1, .eval=T)
expect_true(unlist(x)==4)
expect_true(!is.null(attr(x[[1]], "warn")))

x <- do.rep(f=patr1ckm:::fgWE, list(.f=f, .g=g2, f.args=le(f.args,1), g.args=c(le(ng,1)[2])), 
            .reps=1,.verbose=0, .rep.cores=1, .eval=T)
expect_true(is.na(unlist(x)))
expect_true(!is.null(attr(x[[1]], "err")))

## Test do.fg. This should deal with ng, g.args, and list(g1,g2)

x <- do.fg(le(f.args,2), .f = f, .g = list(g1, g2), g.args = g.args, .nongrid = ng, .reps=1)
expect_true(all(unlist(x) == c(3,4)))
xl <- le(le(x,1),1)
expect_true(all(unlist(lapply(xl, function(x){!is.null(attr(x, "warn"))}))))
expect_true(all(unlist(x) == c(3,4)))

x <- do.fg(le(f.args,1), .f = f, .g = list(g1, g2), g.args = g.args, .nongrid = ng, .reps=1)
expect_true(all(is.na(unlist(x))))
xl <- le(le(x,1),1)
expect_true(all(unlist(lapply(xl, function(x){!is.null(attr(x, "err"))}))))

## Finally, test gcompose
## args has 2 f grid argumens and 1 g grid argument sent to one function



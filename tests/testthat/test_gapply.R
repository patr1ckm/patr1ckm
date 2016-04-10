
context("gapply")

## Single unnamed return value
## names assigned by as.data.frame (V1)
do.one <- function(a=1,b=2){a+b}
r <- do.rep(do.one,list(a=1,b=2), .reps=2)
expect_equal(length(r), 2)
expect_is(r, "list")
expect_null(names(r))
r <- do.rep(do.one,list(a=1,b=2),.reps=8, .verbose=0, .rep.cores=1)
expect_equal(length(r), 8)
expect_null(names(r))

do.one <- function(a=1,b=2){c(a+b, a-b)}
r <- do.rep(do.one,list(a=1,b=2), .reps=2)
expect_null(names(r))

do.one <- function(a=1,b=2){
  if(a==1) stop("asdf")
  a
}
r <- do.rep(do.one,list(a=1,b=2), .reps=2)
expect_is(r[[1]], "try-error")
out <- gapply(do.one, a=1:2, b=2, .reps=2, .verbose=0)
expect_is(attr(out, "err")[[1]], "character")


do.one <- function(a=1,b=2){c(a+b)}
out <- gapply(do.one, a=1:2, b=2, .reps=2, .verbose=0)
grid <- expand.grid(a=1:2,b=2)
expect_equal(colnames(out), c("a", "b", "rep","key","value"))
expect_equal(nrow(out), nrow(grid)*2)
expect_equivalent(out$value,c(3,3,4,4))
expect_equivalent(unique(out[,c("a","b")]), grid)

## Multiple unnamed return values
## Names should be assigned by as.data.frame rules (V1, V2)
do.one <- function(a=1,b=2){c(a+b,a-b)}
out <- gapply(do.one,.reps=2, a=1:2,b=2,.verbose=0)
expect_equal(colnames(out),c("a","b","rep", "key", "value"))
expect_equivalent(unique(out[,c("a","b")]), grid)
expect_equal(unique(out$key),factor(c("V1","V2"),levels=c("V1","V2")))

## Multiple named return values
do.one <- function(a=1,b=2){data.frame(sum=a+b,sub=a-b)}
out <- gapply(do.one,.reps=2, a=1:2,b=2,.verbose=0)
expect_equal(colnames(out),c("a","b","rep","key","value"))
# Test that key is a factor, and has the correct levels
expect_equal(unique(out$key),factor(c("sum","sub"),levels=c("sum","sub")))

## One row in param.grid with multiple reps
do.one <- function(a=1,b=2){data.frame(sum=a+b,sub=a-b)}
out <- gapply(do.one,.reps=3, a=1,b=2,.verbose=0)
expect_equal(unique(out$rep),1:3)
expect_equal(out$value,c(3,3,3,-1,-1,-1))


## .verbose
expect_output(out <- gapply(do.one,.reps=2, a=1:3,b=2:5,.verbose=1), ".")
expect_output(out <- gapply(do.one,.reps=2, a=1:3,b=2:5,.verbose=2), "a = ")
expect_output(out <- gapply(do.one,.reps=2, a=1:3,b=2:5,.verbose=3), "sum sub")

## elapsed time
do.one <- function(a=1,b=2){Sys.sleep(1); return(1)}
out <- gapply(do.one,.reps=2,.verbose=1, a=1,b=1)
expect_is(attr(out,"time"), "proc_time")
attributes(out)

do.one <- function(a=1,b=2){data.frame(sum=a+b,sub=a-b)}
out <- gapply(do.one,.reps=3, a=1,b=2,.verbose=0)
x <- capture.output(o <- summary(out))
expect_output(summary(out), "Source:")
expect_output(summary(out), "Estimated time")
expect_output(summary(out), "Number of conditions: ")
x <- summary(out)
expect_is(x,"tbl")
expect_equal(dim(x), c(2,5))

# Test eval
do.one <- function(a=1,b=2){data.frame(sum=a+b,sub=a-b)}
out <- do.rep(do.one,list(a=1,b=2), .reps=3, .verbose=0, .eval=F)
expect_equal(unlist(out), rep(NA,3))
out <- gapply(do.one,.reps=3, a=1,b=2,.verbose=0, .eval=F)
expect_equal(unlist(out$value), rep(NA,3))

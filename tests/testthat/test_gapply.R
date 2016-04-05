
context("gapply")

## Single unnamed return value
## names assigned by as.data.frame (V1)
do.one <- function(a=1,b=2){a+b}
r <- do.rep(do.one,reps=2,list(a=1,b=2))
expect_equal(colnames(r), "V1")
expect_equal(nrow(r),2)
r <- do.rep(do.one,reps=8,list(a=1,b=2))
expect_equal(nrow(r),8)
expect_equivalent(unique(r),3)

out <- gapply(do.one,reps=2, a=1:2,b=2)
grid <- expand.grid(a=1:2,b=2)
expect_equal(colnames(out), c("param.id","a", "b", "rep","key","value"))
expect_equal(nrow(out), nrow(grid)*2)
expect_equivalent(out$value,c(3,3,4,4))
expect_equivalent(unique(out[,c("a","b")]), grid)

## Multiple unnamed return values
## Names should be assigned by as.data.frame rules (V1, V2)
do.one <- function(a=1,b=2){c(a+b,a-b)}
r <- do.rep(do.one,reps=2,list(a=1,b=2))
expect_equal(colnames(r), c("V1","V2"))

out <- gapply(do.one,reps=2, a=1:2,b=2)
expect_equal(colnames(out),c("param.id","a","b","rep", "key", "value"))
expect_equivalent(unique(out[,c("a","b")]), grid)
expect_equal(unique(out$key),factor(c("V1","V2"),levels=c("V1","V2")))

## Multiple named return values
do.one <- function(a=1,b=2){data.frame(sum=a+b,sub=a-b)}
out <- gapply(do.one,reps=2, a=1:2,b=2)
expect_equal(colnames(out),c("param.id","a","b","rep","key","value"))
# Test that key is a factor, and has the correct levels
expect_equal(unique(out$key),factor(c("sum","sub"),levels=c("sum","sub")))

## verbose
expect_output(out <- gapply(do.one,reps=2, a=1:3,b=2:5,verbose=1), ".")
expect_output(out <- gapply(do.one,reps=2, a=1:3,b=2:5,verbose=2), "a = ")
expect_output(out <- gapply(do.one,reps=2, a=1:3,b=2:5,verbose=3), "c()")

## elapsed time
out <- gapply(do.one,reps=2, a=1:3,b=2:5,verbose=1)
expect_is(attr(out,"time"), "proc_time")
attributes(out)

expect_output(summary(out), "Source:")
expect_output(summary(out), "Estimated time")
o <- summary(out)

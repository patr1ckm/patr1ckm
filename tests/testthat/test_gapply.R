
do.one <- function(a=1,b=2){a+b}
r <- do.rep(do.one,reps=2,list(a=1,b=2))
expect_equal(colnames(r), "V1")
expect_equal(nrow(r),2)
r <- do.rep(do.one,reps=8,list(a=1,b=2))
expect_equal(nrow(r),8)
expect_equivalent(unique(r),3)

out <- gapply(do.one,reps=2, a=1:2,b=2)
grid <- expand.grid(a=1:2,b=2)
expect_equal(colnames(out), c("param.id","a", "b", "V1"))
expect_equal(nrow(out), nrow(grid)*2)
expect_equivalent(out$V1,c(3,3,4,4))
expect_equivalent(unique(out[,c("a","b")]), grid)

do.one <- function(a=1,b=2){c(a+b,a-b)}
r <- do.rep(do.one,reps=2,list(a=1,b=2))
expect_equal(colnames(r), c("V1","V2"))
out <- gapply(do.one,reps=2, a=1:2,b=2)
expect_equal(colnames(out),c("param.id","a","b","V1", "V2"))

do.one <- function(a=1,b=2){data.frame(sum=a+b,sub=a-b)}
(out <- gapply(do.one,reps=2, a=1:2,b=2))
expect_equal(colnames(out),c("param.id","a","b","sum","sub"))

## add reps
f <- do.one
reps <- 2
param.grid <- expand.grid(a=1:2,b=2)
mc.cores <- 1

do.one <- function(a=1,b=2){a+b}
do.rep(do.one,reps=2,list(a=1,b=2))

(out <- gapply(do.one,reps=2, a=1:2,b=2))

do.one <- function(a=1,b=2){c(a+b,a-b)}
do.rep(do.one,reps=2,list(a=1,b=2))
(out <- gapply(do.one,reps=2, a=1:2,b=2))


do.one <- function(a=1,b=2){data.frame(sum=a+b,sub=a-b)}
(out <- gapply(do.one,reps=2, a=1:2,b=2))

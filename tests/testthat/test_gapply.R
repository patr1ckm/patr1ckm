
do.one <- function(a=1,b=2){a+b}
gapply(do.one,reps=5, a=1:4,b=2:3)

do.one <- function(a=1,b=2){data.frame(sum=a+b,sub=a-b)}
gapply(do.one,reps=5, a=1:4,b=2:3)

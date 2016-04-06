
do.one <- function(a=1,b=2){c(a+b,a-b)}
out <- gapply(do.one,reps=2, a=1:2,b=2,verbose=0)

system("mkdir tests/tmp")
system("rm -rf tests/tmp/*")

setup(out, dir="tests/tmp/", nreps = 5)

## This is a bad hack to get the tests to run from this directory
setwd("tests/tmp")
system("Rscript doone.R 1 ")
system("Rscript doone.R 2 ")
setwd("../../")

setup(out, dir="tests/tmp/", nreps = 5, verbose=2)
setwd("tests/tmp")
system("Rscript doone.R 1 ")
system("Rscript doone.R 2 ")
setwd("../../")

setup(out, dir="tests/tmp/", nreps = 5, verbose=3)
setwd("tests/tmp")
system("Rscript doone.R 1 ")
system("Rscript doone.R 2 ")
setwd("../../")


load("tests/tmp/results/cond_1.Rdata")
load("tests/tmp/results/cond_2.Rdata")

#system("rm -rf tests/tmp/*")

out <- collect("tests/tmp/")
summary(out)



x <- c(1,1,1,2,2,3)
check <- matrix(c(1,1,1,1,1,1,2,2,1,2,1,3,1,2,3,1),4,4)
expect_equal(vec2sym(x,diag=1,lower=T,byrow=F), check) # lower tri by col
expect_equal(vec2sym(x,diag=1,lower=F,byrow=T), check) # upper tri by row

x <- c(1,2,2,3,3,3)
check <- matrix(c(1,1,2,3,1,1,2,3,2,2,1,3,3,3,3,1),4,4)
expect_equal(vec2sym(x,diag=1,lower=T,byrow=T),check) # lower tri by row
expect_equal(vec2sym(x,diag=1,lower=F,byrow=F),check) # upper tri by col

expect_equal(diag(vec2sym(x,diag=0,lower=F,byrow=F)),rep(0,4)) # check diag argument


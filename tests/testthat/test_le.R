context("le, ule")
x <- list(list(1,2,3),list(1,2,3,4,5))

expect_is(le(x),"list")
expect_is(ule(x),"numeric")

expect_is(le(x,2),"list")
expect_is(ule(x,2),"numeric")


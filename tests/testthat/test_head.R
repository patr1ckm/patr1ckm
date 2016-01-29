
x <- matrix(1:100,100,9)
xl <- list(1:3,1:4)
xdf <- as.data.frame(x)

expect_equal(dim(hh(x,3)),c(3,3))
expect_equal(length(hh(1:3)),3)
expect_equal(length(hh(xl)),2)

expect_true(any(sh(1:9) != 1:9))
expect_equal(sh(1:3,rows=1:3),1:3)
expect_equal(dim(sh(x,nrow=10,ncol=3)),c(10,3))
expect_equal(length(sh(xl)),2)

expect_equal(hstr(xl),str(xl))
expect_equal(hstr(list(1:3)),str(list(1:3)))
expect_equal(hstr(1:3),str(1:3))
hstr(1:3,rows=1:3)
hstr(xdf,nrow=10,ncol=3)

sstr(xl)
sstr(list(1:3))
sstr(1:3)
sstr(1:3,rows=1:3)
sstr(x)
sstr(xdf)

ht(x)
ht(xdf)
ht(xl)

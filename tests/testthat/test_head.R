
context("list")
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

sink(expect_equal(hstr(xl),str(xl)),file=NULL,type="message")
sink(expect_equal(hstr(list(1:3)),str(list(1:3))),file=NULL,type="message")
sink(expect_equal(hstr(1:3),str(1:3)),file=NULL,type="message")
sink(expect_output(hstr(1:3,rows=1:3),""),file=NULL,type="message")
sink(expect_output(hstr(xdf,nrow=10,ncol=3),""),file=NULL,type="message")

expect_output(sstr(xl),"")
expect_output(sstr(list(1:3)),"")
expect_output(sstr(1:3),"")
expect_output(sstr(1:3,rows=1:3),"")
expect_output(sstr(x),"")
expect_output(sstr(xdf),"")

expect_output(ht(x),"")
expect_output(ht(xdf),"")
expect_output(ht(xl),"")

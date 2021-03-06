context("is.special")

expect_true(is.special(NA))
expect_true(is.special(NaN))
expect_true(is.special(Inf))
expect_true(is.special(-Inf))

#expect_true(is.special(NULL))
expect_true(!is.special(1))
expect_true(!is.special(TRUE))
expect_true(!is.special(as.factor(1)))
expect_true(!is.special("a"))
expect_true(!is.special(FALSE))
expect_true(!is.special(1.0))

# patr1ckm
A few handy R functions

    devtools::install_github("patr1ckm/patr1ckm")

### Printing big objects
- `hh` - return a small number of list elements, rows and/or columns
- `ht` - return a small number of non-overlapping rows and columns from top/bottom and front/back of matrix or front and back elements in a list
- `sh` - return a sample of rows and/or columns
- `hstr` - compute `str` on the first n list elements or columns
- `sstr` - compute `str` on a sample of the first `n` list elements or columns

### Working with lists
- `le` - return the `i`th element from each element of a nested list (i.e. x[[1]][[i]])
- `ule` - return the `i`th element from each element of a nested list, and unlist

### Handy R idioms
- `lsos` - print objects and sizes (from `dmisc`, others)
- `applyna` - apply `is.na` to margins of a matrix
- `is.special` - check if `x` is `NA`, `NaN`, or `Inf`
- `fac2num` - convert factors to numbers (from `kbroman`)
- `vec2sym` - create a symmetric matrix from a vector of the elements of the upper or lower triangle, by row or column

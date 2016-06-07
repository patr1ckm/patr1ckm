# patr1ckm
A few handy R functions

    devtools::install_github("patr1ckm/patr1ckm")


## Simulations
- `gapply` apply the function `f` `reps` times over grid of arguments in parallel
- `setup`, `collect`, `submit` for distributed computing using Sun Grid Engine

### Handy R idioms
- `lsos` - print objects and sizes (from `dmisc`, others)
- `applyna` - apply `is.na` to margins of a matrix
- `is.special` - check if `x` is `NA`, `NaN`, or `Inf`
- `fac2num` - convert factors to numbers (from `kbroman`)
- `vec2sym` - create a symmetric matrix from a vector of the elements of the upper or lower triangle, by row or column

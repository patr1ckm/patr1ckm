# patr1ckm
A few handy R functions

    devtools::install_github("patr1ckm/patr1ckm")

### Printing big objects
- `hh` - return a small number of list elements, rows and/or columns
- `ht` - return a small number of non-overlapping rows and columns from top/bottom and 

### Plots
- `cbb`, `cb` lists of colorblind friendly colors

### Data Analysis
- `applyna` - apply `is.na` to margins of a matrix
- `pw.tables` - compute pairwise tables for all pairs of variables

### Development
- `reup` re-install an already loaded package from github
- `push` add, commit, and push changes to tracked files to github
- `commit` add and commit changes to tracked files

### Other R idioms
- `lsos` - print objects and sizes (from `dmisc`, others)
- `clear.warnings` - to clear warnings
- `put.args` - put the default arguments of a function into an environment (default .GlobalEnv)
- `is.special` - check if `x` is `NA`, `NaN`, or `Inf`
- `fac2num` - convert factors to numbers (from `kbroman`)
- `vec2sym` - create a symmetric matrix from a vector of the elements of the upper or lower triangle, by row or column

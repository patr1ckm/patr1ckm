is.special <- function(x){
  if (is.numeric(x)) !is.finite(x) else is.na(x)
}

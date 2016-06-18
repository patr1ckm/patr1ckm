#' clear warnings
#' @export
clear.warnings <- function() { assign("last.warning", NULL, envir = baseenv()) }

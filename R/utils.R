# General imports
#' @import dplyr
#' @importFrom stringr str_c
#' @importFrom stats na.omit
NULL


#' Remove empty columns from a data.frame
#'
#' Useful to remove unmapped columns extracted from EcoTaxa, which are all empty
#' @param x a data.frame
#' @examples
#' x <- data.frame(a=c(1,NA), b=NA, c=c(NA, "foo"))
#' x
#' remove_empty_cols(x)
#' @export
remove_empty_cols <- function(x) {
  x[, sapply(x, function(X) {!all(is.na(X))})]
}

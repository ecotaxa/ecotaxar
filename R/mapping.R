#' Parse a mapping specification into a named vector
#'
#' @param mapping an EcoTaxa mapping string
#' @examples
#' mapping <- "n01=area\nn02=mean\nn03=stddev"
#' parse_mapping(mapping)
#' @export
parse_mapping <- function(mapping) {
  # reformat mapping for dplyr::rename
  mapping <- stringr::str_split(mapping, "\n|=")[[1]]
  mapping_names <- mapping[seq(2, length(mapping), by=2)]
  mapping <- mapping[seq(1, length(mapping), by=2)]
  names(mapping) <- mapping_names
  return(mapping)
}

#' Parse a mapping specification and rename columns
#'
#' @param x a data.frame with un-mapped columns (in the form "n01", "n02", etc.)
#' @inheritParams parse_mapping
#' @examples
#' d <- data.frame(id=c(1234, 1235), n01=c(12,34), n02=c(125,156))
#' mapping <- "n01=area\nn02=mean\nn03=stddev"
#' map_names(d, mapping)
#' @export
map_names <- function(x, mapping) {
  # parse mapping
  mapping <- parse_mapping(mapping)
  # reduce mapping to columns that are actually there
  mapping <- mapping[mapping %in% names(x)]
  # rename columns
  dplyr::rename(x, UQS(mapping))
}
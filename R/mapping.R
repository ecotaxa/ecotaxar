#' Parse a mapping specification
#'
#' @rdname mapping
#' @param x a data.frame with un-mapped columns (in the form "n01", "n02", etc.)
#' @param mapping an EcoTaxa mapping string (see examples)
#' @examples
#' d <- data.frame(id=c(1234, 1235), n01=c(12,34), n02=c(125,156))
#' mapping <- "n01=area\nn02=mean\nn03=stddev"
#' map_names(d, mapping)
#' parse_mapping(mapping)
#' @export
map_names <- function(x, mapping) {
  # parse mapping
  mapping <- parse_mapping(mapping)
  # reduce mapping to columns that are actually there
  # NB: names() does not work for db sources; colnames() is required.
  mapping <- mapping[mapping %in% colnames(x)]
  # rename columns
  dplyr::rename(x, rlang::UQS(mapping))
}

#' @export
#' @rdname mapping
parse_mapping <- function(mapping) {
  # reformat mapping for dplyr::rename
  mapping <- stringr::str_split(mapping, "\n|=")[[1]]
  mapping_names <- mapping[seq(2, length(mapping), by=2)]
  mapping <- mapping[seq(1, length(mapping), by=2)]
  names(mapping) <- mapping_names
  return(mapping)
}


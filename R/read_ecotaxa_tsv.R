#' Read tsv file from EcoTaxa
#'
#' @param file path to the tsv file
#' @param col_types a column specification for \code{\link[readr]{read_tsv}}, build with \code{\link[readr]{cols}}, or "auto" for automatic parsing
#' @param ... passed to \code{\link[readr]{read_tsv}}
#'
#' @examples
#' d <- read_ecotaxa_tsv(file=system.file("extdata", "export.tsv", package="ecotaxar"))
#' glimpse(d)
#' @export
read_ecotaxa_tsv <- function(file, col_types="auto", ...) {
  if (class(col_types) == "col_spec") {
    columns <- col_types
  } else if (is.character(col_types) & length(col_types) == 1 & col_types == "auto") {
    columns <- readr::cols(
      object_date=readr::col_date(format="%Y%m%d"),
      object_annotation_date=readr::col_date(format="%Y%m%d"),
      object_time=readr::col_time(format="%H%M%S"),
      object_annotation_time=readr::col_time(format="%H%M%S"),
      .default=readr::col_guess()
    )
  } else {
    stop("col_types needs to be a column specification by cols() or \"auto\"")
  }
  readr::read_tsv(file, col_types=columns, ...)
}

#' @export
#' @rdname read_ecotaxa_tsv
read_etx <- read_ecotaxa_tsv
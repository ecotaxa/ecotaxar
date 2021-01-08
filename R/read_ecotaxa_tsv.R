#' Read tsv file from EcoTaxa
#'
#' @param file path to the tsv file.
#' @param col_types a column specification for [readr::read_tsv()], build with [readr::cols()], or "auto" for automatic parsing.
#' @param ... passed to \code{\link[readr]{read_tsv}}.
#'
#' @examples
#' d <- read_ecotaxa_tsv(file=system.file("extdata", "export.tsv", package="ecotaxar"))
#' glimpse(d)
#' d <- read_ecotaxa_tsv(file=system.file("extdata", "import.tsv", package="ecotaxar"))
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
  # NB: skip the optional second line of column numeric/text specification
  readr::read_tsv(file, col_types=columns, comment="[", ...)
}

#' @rdname read_ecotaxa_tsv
#' @export
read_etx <- read_ecotaxa_tsv

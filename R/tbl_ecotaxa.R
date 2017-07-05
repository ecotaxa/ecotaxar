#' Connect to EcoTaxa tables
#'
#' @param db a connection to the EcoTaxa database (from \code{\link{src_ecotaxa}})
#' @export
tbl_ecotaxa <- function(db) {
  for (table in c("projects", "samples", "acquisitions", "process", "objects", "obj_head", "images", "taxonomy")) {
    assign(table, tbl(db, table), envir=globalenv())
  }
  return(invisible(NULL))
}
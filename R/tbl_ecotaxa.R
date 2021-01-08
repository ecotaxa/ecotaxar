#' Connect to common EcoTaxa tables
#'
#' @param db a connection to an EcoTaxa database (from [db_connect_ecotaxa()]).
#'
#' @return The function returns nothing but creates objects in the global environment that are connections to tables through the [dplyr::tbl()] function.
#'
#' @export
#' @examples
#' db <- db_connect_ecotaxa()
#' ls()
#' tbl_ecotaxa(db)
#' ls()
#' head(projects)
tbl_ecotaxa <- function(db) {
  for (table in c("projects", "samples", "acquisitions", "process", "objects", "obj_head", "images", "taxonomy")) {
    assign(table, tbl(db, table), envir=globalenv())
  }
  return(invisible(NULL))
}

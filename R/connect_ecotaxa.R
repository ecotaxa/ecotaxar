#' Connect to the database of EcoTaxa
#'
#' @export
src_ecotaxa <- function() {
  # connect to db
  db <- dplyr::src_postgres(dbname="ecotaxa", host="193.49.112.43", user="zoo", password="z004ecot@x@")
  return(db)
}

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

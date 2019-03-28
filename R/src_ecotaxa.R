#' Database connection to EcoTaxa
#'
#' Connect to and disconnect from the EcoTaxa database (in read-only mode).
#'
#' @examples
#' db <- src_ecotaxa()
#' db
#' db_disconnect(db)
#' # NB: always disconnect after use. Leaving open connections clobbers the server
#' @export
src_ecotaxa <- function() {
  db <- dplyr::src_postgres(dbname="ecotaxa", host="ecotaxa.obs-vlfr.fr", user="zoo", password="z004ecot@x@")
  return(db)
}

#' @rdname src_ecotaxa
#' @export
db_disconnect <- function(x) {
  RPostgreSQL::dbDisconnect(db$con)
}

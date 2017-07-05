#' Connect to the database of EcoTaxa
#'
#' @examples
#' db <- src_ecotaxa()
#' db
#' @export
src_ecotaxa <- function() {
  db <- dplyr::src_postgres(dbname="ecotaxa", host="193.49.112.43", user="zoo", password="z004ecot@x@")
  return(db)
}

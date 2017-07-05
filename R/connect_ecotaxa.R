#' Connect to the database of EcoTaxa
#'
#' @export
src_ecotaxa <- function() {
  # connect to db
  db <- dplyr::src_postgres(dbname="ecotaxa", host="193.49.112.43", user="zoo", password="z004ecot@x@")
  return(db)
}

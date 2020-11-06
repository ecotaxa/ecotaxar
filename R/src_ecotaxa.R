#' Database connection to EcoTaxa
#'
#' Connect to and disconnect from the EcoTaxa database (in read-only mode).
#'
#' @param x a database connection created by [db_connect_ecotaxa()]
#'
#' @return An object of class
#' @export
#'
#' @examples
#' db <- db_connect_ecotaxa()
#' db
#' db_disconnect(db)
#' # NB: always disconnect after use. Leaving open connections clobbers the server
db_connect_ecotaxa <- function() {
  db <- RPostgreSQL::dbConnect("PostgreSQL", host="ecotaxa.obs-vlfr.fr", dbname="ecotaxa", user="zoo", password="z004ecot@x@")
  return(db)
}

# Define a print method for a connection object
setMethod(
  f = "show",
  signature = "PostgreSQLConnection",
  definition = function(object){
    tables <- RPostgreSQL::dbListTables(object)
    tables <- sort(tables)
    tables <- str_c(tables, collapse=", ")
    cat("tbls:", tables)
  }
)

#' @rdname db_connect_ecotaxa
#' @export
db_disconnect_ecotaxa <- function(x) {
  RPostgreSQL::dbDisconnect(x$con)
}

#' @rdname db_connect_ecotaxa
#' @export
src_ecotaxa <- function() {
  .Deprecated("db_connect_ecotaxa")
  db_connect_ecotaxa()
}

#' @rdname db_connect_ecotaxa
#' @export
db_disconnect <- function(x) {
  .Deprecated("db_disconnect_ecotaxa")
  db_disconnect_ecotaxa(x)
}

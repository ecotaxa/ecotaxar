#' Database connection to EcoTaxa
#'
#' Connect to and disconnect from the EcoTaxa database (in read-only mode and only from the local network of the machine running EcoTaxa).
#'
#' @param host URL of the machine hosting EcoTaxa; when NULL, the default, the function will try the URLs of the various hosts in domain obs-vlfr.fr.
#' @param dbname name of the database to connect to.
#' @param user name of a user that has read-only access to the database.
#' @param password password of that user.
#' @param x a database connection created by [db_connect_ecotaxa()].
#' @param ... passed to [DBI::dbConnect()] for a PostgreSQL connection.
#'
#' @return An object of class [RPostgreSQL::PostgreSQLConnection-class()].
#' @export
#'
#' @examples
#' db <- db_connect_ecotaxa()
#' db
#' db_disconnect_ecotaxa(db)
#' # NB: always disconnect after use. Leaving open connections clobbers the server
db_connect <- function(host=NULL, dbname=NULL, user=NULL, password=NULL, ...) {
  db <- RPostgreSQL::dbConnect("PostgreSQL", host=host, dbname=dbname, user=user, password=password, ...)
  return(db)
}

#' @rdname db_connect
#' @export
db_connect_ecotaxa <- function(...) {
  db <- NULL
  # try the usual Villefranche URLs
  # start by the mirror on niko
  tryCatch(
    db <- db_connect(host="niko.obs-vlfr.fr", dbname="ecotaxa3", user="zoo", password="z004ecot@x@", ...),
    error=function(e) {
      warning("Database copy on niko unaccessible, falling back on the original one", call.=FALSE)
    }
  )
  # then fall back on the original database
  if (is.null(db)) {
    db <- db_connect(host="ecotaxa.obs-vlfr.fr", dbname="ecotaxa", user="zoo", password="z004ecot@x@", ...)
  }
  return(db)
}

#' @rdname db_connect
#' @export
db_connect_ecopart <- function(...) {
  db <- db_connect(host="ecotaxa.obs-vlfr.fr", dbname="ecopart", user="zoo", password="z004ecot@x@", port=5435, ...)
  return(db)
}

# Define a print method for a connection object
methods::setMethod(
  f = "show",
  signature = "PostgreSQLConnection",
  definition = function(object){
    tables <- RPostgreSQL::dbListTables(object)
    tables <- sort(tables)
    tables <- str_c(tables, collapse=", ")
    cat("tbls:", tables)
  }
)

#' @rdname db_connect
#' @export
db_disconnect <- function(x) {
  RPostgreSQL::dbDisconnect(x)
}
#' @rdname db_connect
#' @export
db_disconnect_ecotaxa <- db_disconnect
#' @rdname db_connect
#' @export
db_disconnect_ecopart <- db_disconnect

#' @rdname db_connect
#' @export
src_ecotaxa <- function() {
  .Deprecated("db_connect_ecotaxa")
  db_connect_ecotaxa()
}

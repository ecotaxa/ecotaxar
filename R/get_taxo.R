#' Extract a subset of the taxonomy for a set of ids
#'
#' @inheritParams tbl_ecotaxa
#' @param ids vector of numerical ids of taxonomic classes
#' @param recursive when \code{TRUE}, extract the full taxonomy, until the root
get_taxo <- function(db, ids, recursive=FALSE) {
  # reduce to unique taxa
  ids <- unique(ids)
  # if there is only one taxon, duplicate it for the IN sql command to work
  if (length(ids) == 1) { ids <- rep(ids, 2) }

  if (recursive) {
    # get full taxonomy until the root from all taxa
    # (with some recursive SQL-fu!)
    taxo <- dplyr::tbl(db, dplyr::sql(stringr::str_c(
      "WITH RECURSIVE rq AS (
        SELECT DISTINCT t.id,t.name,t.parent_id
        FROM taxonomy t WHERE t.id IN (", stringr::str_c(ids, collapse=","), ")
      union
        SELECT t.id,t.name,t.parent_id
        FROM rq JOIN taxonomy t ON rq.parent_id = t.id
      )
      SELECT * FROM rq")))
    taxo <- dplyr::collect(taxo)
  } else {
    # get taxonomy reduced to the observed taxa
    taxo <- dplyr::tbl(db, "taxonomy") %>%
      dplyr::filter(id %in% ids) %>%
      dplyr::select(-dplyr::starts_with("nbrobj")) %>%
      dplyr::collect()
  }

  # check, a posteriori, that we have at least all the input ids
  # NB: doing it a posteriori avoids having to pay the cost to search the taxonomy before extracting it
  ids <- na.omit(ids)
  ids_ok <- ids %in% taxo$id
  if (!all(ids_ok)) {
    stop("Some ids in `ids` could not be found: ", stringr::str_c(unique(ids[!ids_ok]), collapse=", "))
  }

  # give it a special class to do stuff with
  taxo <- as.data.frame(taxo)
  class(taxo) <- c("taxo", class(taxo))

  return(taxo)
}

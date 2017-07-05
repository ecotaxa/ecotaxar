#' Get the names of taxa from their ids
#'
#' @param ids vector of numeric taxonomic ids
#' @param taxo taxonomy data.frame, with columns \code{id} and \code{name} at least, typically from \code{\link{get_taxo}}
#' @export
taxo_name <- function(ids, taxo) {
  taxo$name[match(ids, taxo$id)]
}

#' Get the full lineage of a taxon from its id
#'
#' @param id numeric taxonomic id
#' @inheritParams taxo_name
#' @export
#' @rdname taxo_name
lineage <- function(id, taxo) {
  if (length(id)!=1) {
    stop("lineage requires a single taxonomic id")
  }
  ancestors(id, taxo, n=Inf) %>% taxo_name(taxo=taxo) %>% str_c(collapse="/")
}

#' Get the ids of taxa from their names
#'
#' @param names vector of character strings with taxonomic names
#' @inheritParams taxo_name
#' @export
#' @rdname taxo_name
taxo_id <- function(names, taxo) {
  taxo$id[match(names, taxo$name)]
}


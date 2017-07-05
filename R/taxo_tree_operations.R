#' Get the vector of ids of ancestors of a taxon
#'
#' @param id numeric ids of taxa (typically a single one)
#' @param taxo taxonomy data.frame, with columns `id` and `parent_id` at least, typically from from \code{\link{get_taxo}} with \code{recursive=TRUE}
#' @param n number of levels to look up; n=1 gives the parent, n=2 gives the grand-parent, etc.
#' @export
ancestors <- function(id, taxo, n=Inf) {
  # checks
  all_ids <- c(taxo$id, taxo$parent_id)
  missing_ids <- setdiff(id, all_ids)
  if ( length(missing_ids) > 0 ) {
    stop("Some ids could not be found: ", str_c(missing_ids, collapse=", "))
  }
  # initialise
  ancestors <- c()
  parent <- id
  count <- -1
  while ( !all(is.na(parent)) & count < n) {
    # add the ancestors
    ancestors <- c(parent, ancestors)
    # the new parents are the parents of the current parents (ouch...)
    parent <- taxo$parent_id[which(taxo$id %in% parent)]
    count <- count + 1
  }
  return(unique(ancestors))
}

#' Get the vector if ids of children of a taxon
#'
#' @inheritParams ancestors
#' @param n number of levels to look down; n=1 gives the direct children, n=2 gives grand children (i.e. children of all children), etc.
#' @export
children <- function(id, taxo, n=Inf) {
  # invert parents and children
  taxo <- dplyr::rename(taxo, id=parent_id, parent_id=id)
  # and get ancestors... which are now children
  rev(ancestors(id=id, taxo=taxo, n=n))
}

#' Tests wether an id corresponds to a leaf in a taxonomic tree
#'
#' @inheritParams ancestors id taxo
is_leaf <- function(id, taxo) {
  length(children(id, taxo, n=1) == 0)
}

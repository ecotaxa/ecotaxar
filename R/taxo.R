
## Extraction ----

#' Extract a subset of the taxonomy for a set of ids
#'
#' @inheritParams tbl_ecotaxa
#' @param ids vector of numerical ids of taxonomic classes
#' @param recursive when \code{TRUE}, extract the full taxonomy, until the root
#'
#' @family taxonomy-related functions
#' @examples
#' db <- src_ecotaxa()
#' extract_taxo(db, 84000:84003)
#' extract_taxo(db, 84000:84003, recursive=FALSE)
#' @export
extract_taxo <- function(db, ids, recursive=TRUE) {
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


## Coercion ----

#' Coerce taxonomy data.frames to various formats
#'
#' @param x a taxonomy data.frame, typically from \code{\link{extract_taxo}}
#' @param ... passed to other methods
#'
#' @examples
#' db <- src_ecotaxa()
#' taxo <- extract_taxo(db, c(10,20))
#' is.taxo(taxo)
#' as.list(taxo)
#' as.Node(taxo)
#' @export
#' @family taxonomy-related functions
is.taxo <- function(x) {
  "taxo" %in% class(x)
}

#' @export
#' @rdname is.taxo
as.list.taxo <- function(x, ...) {
  # compute the number of direct children
  x$n_direct_children <- plyr::laply(x$id, function(i) {length(children(i, x, 1))-1})

  # detect the leaves
  leaves <- filter(x, n_direct_children == 0)

  # for each leaf, compute the path to the root
  paths <- plyr::laply(leaves$id, function(i) {
    anc <- ancestors(i, x)
    anc <- taxo_name(anc, x)
    path <- str_c("`", anc, "`", collapse="$")
    return(path)
  })
  # assign an empty string to each leaf, situated correctly in the list of list
  taxo_list <- list()
  for ( p in paths ) {
    eval(parse(text=str_c("taxo_list$", p," <- ''")))
  }

  # give it a class to do stuff with
  class(taxo_list) <- c("taxo_list", class(taxo_list))

  return(taxo_list)
}

#' @importFrom data.tree as.Node
#' @export
data.tree::as.Node

#' @export
#' @rdname is.taxo
as.Node.taxo <- function(x) {
  x$pathString <- lineage(x$id, x, rooted=TRUE)
  data.tree::as.Node(as.data.frame(x))
}


## Tree traversal ----

#' Get the vector of ids of ancestors of a taxon
#'
#' @param id numerical ids of taxonomic classes (typically a single one)
#' @param taxo taxonomy data.frame, with columns `id` and `parent_id` at least, typically from from \code{\link{get_taxo}} with \code{recursive=TRUE}
#' @param n number of levels to look up; n=1 gives the parent, n=2 gives the grand-parent, etc.
#'
#' @examples
#' db <- src_ecotaxa()
#' taxo <- extract_taxo(db, ids=c(100,200))
#' taxo
#' ancestors(200, taxo)
#' @export
#' @family taxonomy-related functions
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

#' Get the vector of ids of children of a taxon
#'
#' @inheritParams ancestors
#' @param n number of levels to look down; n=1 gives the direct children, n=2 gives grand children (i.e. children of all children), etc.
#' @examples
#' db <- src_ecotaxa()
#' taxo <- extract_taxo(db, ids=c(100,200))
#' taxo
#' children(3, taxo)
#' @export
#' @family taxonomy-related functions
children <- function(id, taxo, n=Inf) {
  # invert parents and children
  taxo <- dplyr::rename(taxo, id=parent_id, parent_id=id)
  # and get ancestors... which are now children
  rev(ancestors(id=id, taxo=taxo, n=n))
}

#' Get id of the parent of a taxon
#'
#' @inheritParams ancestors
#' @examples
#' db <- src_ecotaxa()
#' taxo <- extract_taxo(db, ids=c(100,200))
#' taxo
#' parent(3, taxo)
#' parent(c(200, 3), taxo)
#' @export
#' @family taxonomy-related functions
parent <- function(id, taxo) {
  # ancestors(id, taxo, n=1)[1]
  # or
  taxo$parent_id[match(id, taxo$id)]
}

#' Tests wether an id corresponds to a leaf in a taxonomic tree
#'
#' @inheritParams ancestors
#' @examples
#' db <- src_ecotaxa()
#' taxo <- extract_taxo(db, ids=c(100,200))
#' taxo
#' is_leaf(3, taxo)
#' is_leaf(200, taxo)
#' is_leaf(c(3, 200), taxo)
#' @export
#' @family taxonomy-related functions
is_leaf <- function(id, taxo) {
  sapply(id, function(i) {length(children(i, taxo, n=1)) == 1})
}

#' Get the full lineage of a taxon from its id
#'
#' @inheritParams extract_taxo
#' @inheritParams ancestors
#' @param rooted when \code{TRUE}, add a root (#) to the tree
#' @examples
#' db <- src_ecotaxa()
#' taxo <- extract_taxo(db, ids=c(100,200))
#' taxo
#' lineage(200, taxo)
#' lineage(c(3, 200), taxo)
#' lineage(200, taxo, rooted=T)
#' @export
#' @family taxonomy-related functions
lineage <- function(ids, taxo, rooted=FALSE) {
  sapply(ids, function(id) {
    l <- ancestors(id, taxo, n=Inf) %>% taxo_name(taxo=taxo) %>% str_c(collapse="/")
    if (rooted) {
      l <- str_c("/#/", l)
    }
    return(l)
  })
}


## Convenience functions ----

#' Get the names of taxa from their ids
#'
#' @inheritParams lineage
#' @examples
#' db <- src_ecotaxa()
#' taxo <- extract_taxo(db, ids=c(100,200))
#' taxo
#' taxo_name(200, taxo)
#' taxo_name(1, taxo)
#' @export
#' @family taxonomy-related functions
taxo_name <- function(ids, taxo) {
  taxo$name[match(ids, taxo$id)]
  # TODO add disambiguation here
}

#' Get the ids of taxa from their names
#'
#' @param names vector of character strings with taxonomic names
#' @inheritParams taxo_name
#' @examples
#' db <- src_ecotaxa()
#' taxo <- extract_taxo(db, ids=c(100,200))
#' taxo
#' taxo_id("Foo", taxo)
#' taxo_id("Euzebyaceae", taxo)
#' @export
#' @family taxonomy-related functions
taxo_id <- function(names, taxo) {
  taxo$id[match(names, taxo$name)]
  # TODO deal with synonyms here
}


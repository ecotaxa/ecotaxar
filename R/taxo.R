
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
  # remove NAs in classif ids
  # = unidentified objects which do not matter to compute the taxonomy
  ids <- na.omit(ids)

  # reduce to unique taxa
  ids <- unique(ids)
  
  if (length(ids) == 0) {
    # if all ids are missing, do not extract anything
    taxo <- data.frame(id=NA, parent=NA, name=NA)

  } else {
    # otherwise proceed with the extraction

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
    ids_ok <- ids %in% taxo$id
    if (!all(ids_ok)) {
      stop("Some ids in `ids` could not be found: ", stringr::str_c(unique(ids[!ids_ok]), collapse=", "))
    }
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
#' d <- as.data.frame(taxo)
#' class(d)
#' d <- as.taxo(d)
#' class(d)
#' is.taxo(d)
#' as.Node(d)
#' as.list(d)
#'
#' db <- src_ecotaxa()
#' taxo <- extract_taxo(db, c(8000, 20000))
#' as.Node(taxo)
#' @export
#' @family taxonomy-related functions
as.taxo <- function(x) {
  if (all(c("id", "parent_id", "name") %in% names(x)) & is.data.frame(x)) {
    class(x) <- c("taxo", class(x))
  } else {
    stop("Need a data.frame with at least columns `id`, `parent_id`, and `name`")
  }
  return(x)
}

#' @export
#' @rdname as.taxo
is.taxo <- function(x) {
  "taxo" %in% class(x)
}

#' @export
#' @rdname as.taxo
as.list.taxo <- function(x, ...) {
  # detect the leaves
  leaves <- x$id[is_leaf(x$id, x)]

  # for each leaf, compute the path to the root = the lineage
  paths <- lineage(leaves, x)
  # replace separators by $ to recreated list traversal
  paths <- stringr::str_replace_all(paths, "/", "$")
  # assign an empty string to each leaf, situated correctly in the list of list
  taxo_list <- list()
  for ( p in paths ) {
    eval(parse(text=stringr::str_c("taxo_list$", p," <- ''")))
  }

  # give it a class to do stuff with
  class(taxo_list) <- c("taxo_list", class(taxo_list))

  return(taxo_list)
}

#' @importFrom data.tree as.Node
#' @export
data.tree::as.Node

#' @export
#' @rdname as.taxo
as.Node.taxo <- function(x, ...) {
  x$pathString <- lineage(x$id, x, rooted=TRUE)
  class(x) <- "data.frame"
  data.tree::as.Node(x)
}


## Tree traversal ----

#' Get id of the parent of a taxon
#'
#' @param id numerical ids of taxonomic classes (typically a single one)
#' @param taxo a taxonomy data.frame, typically from \code{\link{extract_taxo}}
#' @examples
#' taxo
#' parent(3, taxo)
#' parent(1:4, taxo)
#' parent(c(5, NA), taxo)
#' @export
#' @family taxonomy-related functions
parent <- function(id, taxo) {
  # check
  unknown_ids <- na.omit(setdiff(id, taxo$id))
  if (length(unknown_ids) != 0) {
    stop("Some taxonomic ids are not in the taxonomy: ", stringr::str_c(unknown_ids, collapse=", "))
  }
  # get parents, in order
  taxo$parent_id[match(id, taxo$id)]
}
#' @rdname parent
#' @export
parents <- parent

#' Get id(s) of the children of a taxon
#'
#' @inheritParams parent
#' @examples
#' taxo
#' children(3, taxo)
#' children(1:4, taxo)
#' children(c(1:4, NA), taxo)
#' children(NA, taxo)
#' @export
#' @family taxonomy-related functions
children <- function(id, taxo) {
  # deal with NAs
  if (any(is.na(id))) {
    # record there was at least one
    children <- NA
    # remove them because they would be mistaken for the taxo root
    id <- na.omit(id)
  } else {
    children <- c()
  }
  # check
  unknown_ids <- setdiff(id, c(taxo$id, taxo$parent_id))
  if (length(unknown_ids) != 0) {
    stop("Some taxonomic ids are not in the taxonomy: ", stringr::str_c(unknown_ids, collapse=", "))
  }
  # get children
  c(children, taxo$id[taxo$parent_id %in% id])
}

#' Get ids of ancestors of a taxon
#'
#' @inheritParams parent
#' @param n number of levels to look up; n=1 gives the parents, n=2 gives the grand-parents, etc.
#' @details Even with `n=1` function is different from `[parent()]` because it returns a vector of unique parents for all input `id` values, not necessarily in order.
#' @examples
#' taxo
#' ancestors(6, taxo)
#' ancestors(5:7, taxo)
#' ancestors(1, taxo)
#' ancestors(NA, taxo)
#' ancestors(6, taxo, n=1)
#' ancestors(6, taxo, n=2)
#' ancestors(6, taxo, n=3)
#' ancestors(6, taxo, n=10)
#' # NB:
#' ancestors(5:7, taxo, n=1)
#' parent(5:7, taxo)
#' @export
#' @family taxonomy-related functions
ancestors <- function(id, taxo, n=Inf) {
  # initialise
  ancestors <- c()
  parents <- id
  count <- 0
  # loop over levels
  while ( length(parents) != 0 & count < n) {
    # the new parents are the parents of the current parents (ouch...)
    parents <- unique(na.omit(parent(parents, taxo)))
    # add to ancestors
    ancestors <- c(parents, ancestors)
    count <- count + 1
  }
  return(unique(ancestors))
}

#' Get ids of descendants of a taxon
#'
#' @inheritParams parent
#' @param @param n number of levels to look down; n=1 gives the direct children, n=2 gives grand-children (i.e. children of all children), etc.
#' @examples
#' taxo
#' descendants(3, taxo)
#' descendants(1:3, taxo)
#' descendants(7, taxo)
#' descendants(NA, taxo)
#' descendants(1, taxo, n=1)
#' descendants(1, taxo, n=2)
#' descendants(1, taxo, n=3)
#' descendants(1, taxo, n=10)
#' @export
#' @family taxonomy-related functions
descendants <- function(id, taxo, n=Inf) {
  # initialise
  descendants <- c()
  children <- id
  count <- 0
  # loop over levels
  while ( length(children) != 0 & count < n) {
    # the new children are the children of the current children (ouch...)
    children <- unique(na.omit(children(children, taxo)))
    # add to ancestors
    descendants <- c(children, descendants)
    count <- count + 1
  }
  return(unique(descendants))
}

#' Tests whether a taxon is a leaf in a taxonomic tree
#'
#' @inheritParams children
#' @examples
#' taxo
#' is_leaf(3, taxo)
#' is_leaf(6, taxo)
#' is_leaf(NA, taxo)
#' is_leaf(c(5,NA,6,NA), taxo)
#' @export
#' @family taxonomy-related functions
is_leaf <- function(id, taxo) {
  # reduce to unique ids to speeds things up
  uid <- unique(id)

  # for each, compute whether it is a leaf
  leaf_bool <- sapply(uid, function(i) {
    if (is.na(i)) {
      NA
    } else {
      length(children(i, taxo)) == 0
    }
  })

  # match with original ids to output a vector of the correct length
  leaf_bool[match(id, uid)]
}

#' Get the full lineage of a taxon
#'
#' @inheritParams parent
#' @param rooted when \code{TRUE}, add a root (#) to the tree
#' @examples
#' taxo
#' lineage(6, taxo)
#' lineage(6, taxo, rooted=TRUE)
#' lineage(c(6, 7), taxo)
#' lineage(NA, taxo)
#' lineage(c(6, NA), taxo)
#' @export
#' @family taxonomy-related functions
lineage <- function(id, taxo, rooted=FALSE) {
  # reduce to unique ids to speeds things up
  uid <- unique(id)

  lineages <- sapply(uid, function(i) {
    # l <- c(ancestors(i, taxo, n=Inf), i) %>% taxo_name(taxo=taxo) %>% stringr::str_c(collapse="/")
    if (rooted) {
      l <- stringr::str_c("/#/", l)
    }
    return(l)
  })

  # match with original ids to output a vector of the correct length
  lineages[match(id, uid)]
}


## Convenience functions ----

#' Get the names of taxa from their ids
#'
#' @inheritParams parent
#' @param unique force names to be unique by adding the parent name when needed
#' @param computer_friendly when TRUE, the final name is made to contain no spaces or special characters; when FALSE the names are left as is and, when they are made unique, the parent name is added in parentheses after the taxon name (like on EcoTaxa).
#' @examples
#' taxo
#' taxo_name(5, taxo)
#' taxo_name(2:6, taxo)
#' taxo_name(2:6, taxo, unique=TRUE)
#' taxo_name(2:6, taxo, unique=TRUE, computer_friendly=TRUE)
#' @export
#' @family taxonomy-related functions
taxo_name <- function(ids, taxo, unique=FALSE, computer_friendly=FALSE) {
  # reduce to unique ids to be faster and able to detect duplicated names
  uids <- unique(ids)

  if (unique) {
    names <- taxo_name(uids, taxo)
    parent_names <- uids %>% parent(taxo) %>% taxo_name(taxo)

    # for duplicated names, add the name of the parent in parentheses
    dup_idx <- which(names %in% names[duplicated(names)])
    names[dup_idx] <- str_c(names[dup_idx], " (", parent_names[dup_idx], ")")

    # TODO this does not solve the problem of non-unique parent-child couples but it does not seem to exist currently
  } else {
    names <- taxo$name[match(uids, taxo$id)]
  }

  if (computer_friendly) {
    names <- names %>%
      stringr::str_replace_all("[ \\.\\(\\)]", " ") %>%
      stringr::str_trim() %>%
      stringr::str_replace_all(" ", "_")
  }

  # extract the names of all ids
  out <- names[match(ids, uids)]

  return(out)
}

#' Get the ids of taxa from their names
#'
#' @param names vector of character strings with taxonomic names
#' @inheritParams taxo_name
#' @examples
#' taxo
#' taxo_id("squid", taxo)
#' taxo_id("foo", taxo)
#' taxo_id("egg", taxo)
#' @export
#' @family taxonomy-related functions
taxo_id <- function(names, taxo) {
  taxo$id[match(names, taxo$name)]
  # TODO deal with synonyms here
}


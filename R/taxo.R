#' Minimal, fake taxonomy table
#'
#' @format A data frame with 7 rows and 3 variables:
#' \describe{
#'   \item{id}{taxon identifier}
#'   \item{parent_id}{identifier of the parent}
#'   \item{name}{name of the taxon (i.e. text)}
#' }
"taxo"

## Extraction ----

#' Extract a subset of the taxonomy for a set of ids
#'
#' @inheritParams tbl_ecotaxa
#' @template param-taxon_ids
#' @param recursive when \code{TRUE}, extract the full taxonomy, until the root.
#'
#' @family taxonomy-related functions
#' @examples
#' db <- db_connect_ecotaxa()
#' extract_taxo(db, 84000:84003)
#' extract_taxo(db, 84000:84003, recursive=FALSE)
#' @export
extract_taxo <- function(db, taxon_ids, recursive=TRUE) {
  # remove NAs in classif ids
  # = unidentified objects which do not matter to compute the taxonomy
  taxon_ids <- na.omit(taxon_ids)

  # reduce to unique taxa
  taxon_ids <- unique(taxon_ids)

  if (length(taxon_ids) == 0) {
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
          FROM taxonomy t WHERE t.id IN (", stringr::str_c(taxon_ids, collapse=","), ")
        union
          SELECT t.id,t.name,t.parent_id
          FROM rq JOIN taxonomy t ON rq.parent_id = t.id
        )
        SELECT * FROM rq")))
      taxo <- dplyr::collect(taxo)
    } else {
      # get taxonomy reduced to the observed taxa
      taxo <- dplyr::tbl(db, "taxonomy") %>%
        dplyr::filter(id %in% taxon_ids) %>%
        dplyr::select(-dplyr::starts_with("nbrobj")) %>%
        dplyr::collect()
    }

    # check, a posteriori, that we have at least all the input ids
    # NB: doing it a posteriori avoids having to pay the cost to search the taxonomy before extracting it
    ids_ok <- taxon_ids %in% taxo$id
    if (!all(ids_ok)) {
      stop("Some ids in `ids` could not be found: ", stringr::str_c(unique(taxon_ids[!ids_ok]), collapse=", "))
    }
  }

  # give it a special class to do stuff with
  taxo <- as.data.frame(taxo)
  class(taxo) <- c("taxo", class(taxo))

  return(taxo)
}


## Coercion ----

#' Coerce a taxonomy data.frame to various formats
#'
#' @param x a taxonomy data.frame, typically from [extract_taxo()].
#' @param ... passed to other methods.
#'
#' @details A taxonomy data.frame is just a regular data.frame with columns `id`, `parent_id`, and `name`. The links between the `id` and `parent_id` define the taxonomic hierarchy.
#' It can be converted into an actual tree using [data.tree::as.Node()] and into a nested list.
#'
#' @family taxonomy-related functions
#' @export
#' @examples
#' taxo
#' as.Node(taxo)
#' print(as.Node(taxo), "id", "parent_id")
#' as.list(taxo)
#'
#' # transforming into a taxonomy data.frame is just a mater of adding a class
#' df <- taxo
#' class(df)
#' is.taxo(df)
#' df <- as.data.frame(df)
#' class(df)
#' is.taxo(df)
#' df <- as.taxo(df)
#' class(df)
#' is.taxo(df)
#'
#' \dontrun{
#' db <- db_connect_ecotaxa()
#' taxo <- extract_taxo(db, c(8000, 20000))
#' as.Node(taxo)
#' }
as.taxo <- function(x) {
  if (all(c("id", "parent_id", "name") %in% names(x)) & is.data.frame(x)) {
    class(x) <- c("taxo", class(x))
  } else {
    stop("Need a data.frame with at least columns `id`, `parent_id`, and `name`")
  }
  return(x)
}

#' @rdname as.taxo
#' @export
is.taxo <- function(x) {
  "taxo" %in% class(x)
}

#' @rdname as.taxo
#' @export
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

#' @rdname as.taxo
#' @export
as.Node.taxo <- function(x, ...) {
  x$pathString <- lineage(x$id, x, rooted=TRUE)
  class(x) <- "data.frame"
  data.tree::as.Node(x)
}


## Tree traversal ----

#' Get id of the parent of some taxa
#'
#' @template param-taxon_ids
#' @param taxo a taxonomy data.frame, typically from [extract_taxo()].
#'
#' @details `parent` and `parents` are just synonyms. The function works per taxon but is vectorised over `taxon_ids`.
#'
#' @return A vector of taxon ids, as long as the input, containing the id of the parent of each element in the input.
#'
#' @family taxonomy-related functions
#' @export
#' @examples
#' print(as.Node(taxo), "id")
#' parent(3, taxo)
#' parent(1:4, taxo)
#' parent(c(5, NA), taxo)
#' \dontrun{ parent(12, taxo) }
parent <- function(taxon_ids, taxo) {
  # check arguments
  checkmate::assert_integerish(taxon_ids)
  unknown_ids <- na.omit(setdiff(taxon_ids, taxo$id))
  if (length(unknown_ids) != 0) {
    stop("Some taxon_ids are not in the taxonomy: ", stringr::str_c(unknown_ids, collapse=", "))
  }

  # get parents, in order
  taxo$parent_id[match(taxon_ids, taxo$id)]
}

#' @rdname parent
#' @export
parents <- parent

#' Get ids of the children of some taxa
#'
#' @inheritParams parent
#' @param simplify when `taxon_ids` contains only one taxon and `simplify` is `TRUE`, return the vector of ids of the children directly, not a list of length one.
#'
#' @details The function works per taxon but is vectorised over `taxon_ids`.
#'
#' @return A list of vectors of taxon ids, as long as the input, containing the ids of the children of each element in `taxon_ids`.
#'
#' @family taxonomy-related functions
#' @export
#' @examples
#' print(as.Node(taxo), "id")
#' children(3, taxo)
#' children(1:4, taxo)
#' children(c(5, NA), taxo)
#' \dontrun{ children(12, taxo) }
children <- function(taxon_ids, taxo, simplify=TRUE) {
  # check arguments
  checkmate::assert_integerish(taxon_ids)
  unknown_ids <- na.omit(setdiff(taxon_ids, taxo$id))
  if (length(unknown_ids) != 0) {
    stop("Some taxon_ids are not in the taxonomy: ", stringr::str_c(unknown_ids, collapse=", "))
  }

  # remove taxa with no parent
  taxo <- taxo[!is.na(taxo$parent_id),]

  # loop over taxa
  # TODO optimise for repeated taxon_ids
  children <- lapply(taxon_ids, function(this_id) {
    if (is.na(this_id)) {
      these_children <- NA
    } else {
      these_children <- taxo$id[taxo$parent_id == this_id]
    }
    return(these_children)
  })

  # simplify if needed
  if (length(taxon_ids) == 1 & simplify) {
    children <- children[[1]]
  }

  return(children)
}

#' Get the ids of all ancestors of some taxa
#'
#' @inheritParams parent
#' @param n maximum number of levels to look up; n=1 gives the parents, n=2 gives the parents and the grand-parents, etc.
#'
#' @details Even with `n=1`, the function is different from [parent()] because it returns all unique parents for all elements of `taxon_ids`, not one parent per element.
#'
#' @return A vector of taxonomic ids containing the input `taxon_ids` and all their ancestors. With a single taxon in `taxon_ids`, the ancestors are in ordered from the taxon to the root of the tree: they define the lineage. When `taxon_ids` contains multiple taxa, the order is non-trivial.
#'
#' @family taxonomy-related functions
#' @export
#' @examples
#' print(as.Node(taxo), "id")
#' ancestors(5, taxo)
#' ancestors(1, taxo)
#' ancestors(NA, taxo)
#' ancestors(1:7, taxo)
#' ancestors(6, taxo, n=1)
#' ancestors(6, taxo, n=2)
#' ancestors(6, taxo, n=3)
#' ancestors(6, taxo, n=10)
#' # NB:
#' ancestors(5:6, taxo, n=1)
#' parent(5:6, taxo)
ancestors <- function(taxon_ids, taxo, n=Inf) {
  # initialise
  ancestors <- taxon_ids
  parents <- taxon_ids
  count <- 0
  # loop over levels
  while ( length(parents) != 0 & count < n) {
    # the new parents are the parents of the current parents (ouch...)
    parents <- unique(na.omit(parent(parents, taxo)))
    # add to ancestors
    ancestors <- unique(c(parents, ancestors))
    count <- count + 1
  }
  return(ancestors)
}

#' Get ids of descendants of some taxa
#'
#' @inheritParams parent
#' @param n maximum number of levels to look down; n=1 gives the direct children, n=2 gives children and grand-children, etc.
#'
#' @details Even with `n=1`, the function is different from [children()] because it returns all unique children for all elements of `taxon_ids`, not one vector of children per element.
#'
#' @return A vector of taxonomic ids containing the input `taxon_ids` and all their descendants.
#'
#' @family taxonomy-related functions
#' @export
#' @examples
#' print(as.Node(taxo), "id")
#' descendants(3, taxo)
#' descendants(1, taxo)
#' descendants(c(2,5), taxo)
#' descendants(7, taxo)
#' descendants(NA, taxo)
#' descendants(1, taxo, n=1)
#' descendants(1, taxo, n=2)
#' descendants(1, taxo, n=3)
#' descendants(1, taxo, n=10)
#' # NB:
#' descendants(5:6, taxo, n=1)
#' children(5:6, taxo)
descendants <- function(taxon_ids, taxo, n=Inf) {
  # initialise
  descendants <- taxon_ids
  children <- na.omit(taxon_ids)
  count <- 0
  # loop over levels
  while ( length(children) != 0 & count < n) {
    # the new children are the children of the current children (ouch...)
    children <- unique(unlist(children(children, taxo)))
    # add to ancestors
    descendants <- c(descendants, children)
    count <- count + 1
  }
  return(unique(descendants))
}

#' Tests whether taxa are leaves in a taxonomic tree
#'
#' @inheritParams parent
#'
#' @family taxonomy-related functions
#' @export
#' @examples
#' print(as.Node(taxo), "id")
#' is_leaf(5, taxo)
#' is_leaf(6, taxo)
#' is_leaf(NA, taxo)
#' is_leaf(c(5,NA,6,NA), taxo)
is_leaf <- function(taxon_ids, taxo) {
  # reduce to unique, non-missing ids to speeds things up
  uid <- na.omit(unique(taxon_ids))

  # for each, compute whether it has 0 children
  leaf_bool <- sapply(children(uid, taxo, simplify=FALSE), length) == 0

  # match with original ids to output a vector of the correct length
  leaf_bool[match(taxon_ids, uid)]
}

#' Get the full lineage of a taxon
#'
#' @inheritParams parent
#' @param rooted when \code{TRUE}, add a root (#) to the lineage path.
#'
#' @return A vector of strings containing the lineage for each element of `taxon_ids`.
#'
#' @family taxonomy-related functions
#' @export
#' @examples
#' print(as.Node(taxo), "id")
#' lineage(6, taxo)
#' lineage(6, taxo, rooted=TRUE)
#' lineage(c(6, 7), taxo)
#' lineage(NA, taxo)
#' lineage(c(6, NA), taxo)
lineage <- function(taxon_ids, taxo, rooted=FALSE) {
  # reduce to unique ids to speeds things up
  uid <- unique(taxon_ids)

  lineages <- sapply(uid, function(i) {
    l <-ancestors(i, taxo, n=Inf) %>% taxo_name(taxo=taxo) %>% stringr::str_c(collapse="/")
    if (rooted) {
      l <- stringr::str_c("/#/", l)
    }
    return(l)
  })

  # match with original ids to output a vector of the correct length
  lineages[match(taxon_ids, uid)]
}


## Convenience functions ----

#' Get the names of taxa from their ids
#'
#' @inheritParams parent
#' @param unique force names to be unique by adding the parent name when needed.
#'
#' @return A vector names, one for each element of `taxon_ids`.
#'
#' @family taxonomy-related functions
#' @export
#' @examples
#' print(as.Node(taxo), "id")
#' taxo_name(5, taxo)
#' taxo_name(2:7, taxo)
#' taxo_name(2:7, taxo, unique=TRUE)
taxo_name <- function(taxon_ids, taxo, unique=FALSE) {
  # reduce to unique ids to be faster and able to detect duplicated names
  uid <- unique(taxon_ids)

  if (unique) {
    names <- taxo_name(uid, taxo)
    parent_names <- uid %>% parent(taxo) %>% taxo_name(taxo)

    # for duplicated names, add the name of the parent
    dup_idx <- which(duplicated(names) | duplicated(names, fromLast=T))
    names[dup_idx] <- stringr::str_c(names[dup_idx], "<", parent_names[dup_idx])
    # TODO this does not solve the problem of non-unique parent-child couples but it does not seem to exist currently
  } else {
    names <- taxo$name[match(uid, taxo$id)]
  }

  # extract the names of all ids
  names[match(taxon_ids, uid)]
}

#' Get the ids of taxa from their names
#'
#' @param taxon_names vector of character strings with taxonomic names
#' @inheritParams taxo_name
#'
#' @return A vector ids, one for each element of `taxon_names`. Currently, this does not disambiguate between taxa with the same name and just returns the id of the first match in the taxonomy.
#'
#' @examples
#' taxo
#' taxo_id("squid", taxo)
#' taxo_id("foo", taxo)
#' NB:
#' taxo_id("egg", taxo)
#' @export
#' @family taxonomy-related functions
taxo_id <- function(taxon_names, taxo) {
  taxo$id[match(names, taxo$name)]
  # TODO need to deal with synonyms here
}


#' Test if the argument is of class taxo
#'
#' @param x an object
#' @export
is.taxo <- function(x) {
  "taxo" %in% class(x)
}

#' Convert the taxonomy data.frame into a nested list, for shinyTree
#'
#' @param x taxonomy data.frame, of class "taxo" (from \code{\link{get_taxo}})
#' @param ... passed to other methods
#' @export
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

#' Convert a taxonomy data.frame into a Node object, from package data.tree
#'
#' @param x taxonomy data.frame, of class "taxo" (from \code{\link{get_taxo}})
#' @export
as.Node.taxo <- function(x) {
  x$pathString <- plyr::laply(x$id, function(i) {
    anc <- ancestors(i, x, n=Inf)
    str_c(taxo_name(anc, x), collapse="/")
  })
  data.tree::as.Node(as.data.frame(x))
}


# # test taxo
# db <- connect_ecotaxa()
#
# (taxo <- extract_taxo(db, 10))
# class(taxo)
#
# (taxo <- extract_taxo(db, c(1000, 1200, 1300, 1400, 1500), recursive = TRUE))
#
# ancestors(1000, taxo)
# ancestors(1000, taxo, n=1)
# ancestors(1000, taxo, n=2)
# taxo_name(ancestors(1000, taxo), taxo)
#
# children(151, taxo)
# children(151, taxo, n=1)
# taxo_name(children(151, taxo, n=1), taxo)
#
# simplify_taxo(taxo)
#
# taxo
# as.list(taxo)
# as.Node(taxo)
#
# extract_taxo(db, 10:12)
# extract_taxo(db, 10^6)
#
# rm(db)

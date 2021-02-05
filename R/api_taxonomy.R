#' Get information about a single taxon
#'
#' @template param-taxon_id
#'
#' @return A list containing
#' - `id` : the numerical id of the taxon,
#' - `display_name` : a unique name for the taxon (in which the parent name gets appended if the taxon name is not unique),
#' - `lineage` : a vector of the taxon name and its ancestors, in order of ascendance.
#' @export
#' @examples
#' api_taxon(1234)
#' api_taxon(85004)
#' api_taxon(84968)
api_taxon <- function(taxon_id) {
  # check the argument
  checkmate::assert_scalar(taxon_id)
  checkmate::assert_integerish(taxon_id)
  # get the info
  apiGET(str_c("taxon/", taxon_id))
}

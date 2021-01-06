#' Get information about a single taxon
#'
#' @param id internal id of the taxon
#'
#' @return A list containing
#' - `id` : the numerical id of the taxon
#' - `display_name` : a unique name for the taxon (in which the parent name gets appended if the taxon name is not unique)
#' - `lineage` : the vector the taxon name and its ancestors, in order of ascendance.
#'
#' @export
#'
#' @examples
#' api_taxon(id=1234)
#' api_taxon(id=85004)
#' api_taxon(id=84968)
api_taxon <- function(id) {
  # check the argument
  checkmate::assert_scalar(id)
  checkmate::assert_integerish(id)
  # get the info
  apiGET(str_c("taxa/", id))
  # TODO change once the endpoint matches the name here
}

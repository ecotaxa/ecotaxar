#' Get information about a single object
#'
#' @param id internal id of the object
#'
#' @return A nested list with the properties of the object.
#'
#' @export
#'
#' @examples
#' info <- api_object(id=24473014)
#' str(info, 1)
api_object <- function(id) {
  # check the argument
  checkmate::assert_scalar(id)
  checkmate::assert_integerish(id)
  # get the info
  apiGET(str_c("object/", id))
}

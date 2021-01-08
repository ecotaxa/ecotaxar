#' Get information about a single object
#'
#' @template param-object_id
#'
#' @return A nested list with the properties of the object.
#' @export
#' @examples
#' info <- api_object(24473014)
#' str(info, 1)
api_object <- function(object_id) {
  # check the argument
  checkmate::assert_scalar(object_id)
  checkmate::assert_integerish(object_id)
  # get the info
  apiGET(str_c("object/", object_id))
}

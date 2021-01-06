#' Get information about a single sample
#'
#' @param id internal id of the sample
#'
#' @return A nested list with the properties of the sample.
#'
#' @export
#'
#' @examples
#' info <- api_sample(id=15709)
#' str(info)
api_sample <- function(id) {
  # check the argument
  checkmate::assert_scalar(id)
  checkmate::assert_integerish(id)
  # get the info
  apiGET(str_c("sample/", id))
}

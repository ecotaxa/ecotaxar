#' Get information about a single sample
#'
#' @template param-sample_id
#'
#' @return A nested list with the properties of the sample.
#' @export
#' @examples
#' info <- api_sample(15709)
#' str(info)
api_sample <- function(sample_id) {
  # check the argument
  checkmate::assert_scalar(sample_id)
  checkmate::assert_integerish(sample_id)
  # get the info
  apiGET(str_c("sample/", sample_id))
}

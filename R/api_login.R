#' Log in EcoTaxa
#'
#' @param username your EcoTaxa username (i.e. email address).
#' @param password your EcoTaxa password, as text.
#' @param cache whether to store the token in a file, determined by [api_token_path()]. This is `TRUE` by default and allows other functions to reuse the token.
#'
#' @return The token string, invisibly. It also stores to token string in the file defined by [api_token_path()] if `cache=TRUE` (the default).
#' @export
#' @examples
#' # log in and get a token
#' token <- api_login(username="ecotaxa.api.user@gmail.com", password="test!")
#' token
#' # check where the token is stored
#' api_token_path()
#' readLines(api_token_path())
api_login <- function(username, password, cache=TRUE) {
  response <- httr::POST(
    url=str_c(api_url(), "login"),
    config=httr::config(ssl_verifypeer=FALSE),
    body=list(username=username, password=password),
    encode="json"
  )
  token <- api_handle_response(response)

  if (cache) {
    token_path <- api_token_path()
    dir.create(dirname(token_path), showWarnings=FALSE)
    writeLines(token, con=token_path)
  }
  return(invisible(token))
}

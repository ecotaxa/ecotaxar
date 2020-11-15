#' Get the path to to the cached EcoTaxa token
#'
#' This path can be changed with option "ecotaxar.token_path"; by default, it is
#' "~/.R/ecotaxar/token"
#'
#' @return The path to the token file.
#'
#' @examples
#' # get current token path
#' api_token_path()
#' # set the option to change the path
#' options(ecotaxa.token_path="~/.my_ecotaxa_token")
#' api_token_path()
#' # NB: write this option in .Rprofile to make it permanent.
#' @export
api_token_path <- function() {
  token_path <- getOption("ecotaxar.token_path")
  if (is.null(token_path)) {
    token_path <- "~/.R/ecotaxar/token"
  }
  token_path <- path.expand(token_path)
  return(token_path)
}

#' Read EcoTaxa's API token
#'
#' @param path path to the file where the token is stored, defined by `[api_token_path()]`
#'
#' @return The token string.
#'
#' @examples
#' # get a token
#' api_login(username="ecotaxa.api.user@gmail.com", password="test!")
#' # and read it
#' api_token()
#' @export
api_token <- function(path=api_token_path()) {
  if (!file.exists(path)) {
    stop("Cannot find API token at ", path)
  }
  token <- readLines(path)
  return(token)
}

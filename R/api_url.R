api_url <- function() {
  url <- getOption("ecotaxa.url")
  # TODO look at ways to save this
  if (is.null(url)) {
    url <- "https://ecotaxa.obs-vlfr.fr/api/"
  }
  return(url)
}

#' Handle an API response
#'
#' Fail, with information, if the status code is not 200. Return the parsed response content otherwise
#'
#' @param x an [httr::response] object
#'
#' @export
api_handle_response <- function(x) {
  content <- httr::content(x, as="parsed")
  if (x$status_code != 200) {
    print(x)
    print(content)
    stop("API answered with status: ", x$status_code, call.=FALSE)
    out <- NULL
  } else {
    out <- content
  }
  return(out)
}

apiGET <- function(endpoint) {
  api_handle_response(
    httr::GET(
      url=str_c(api_url(), endpoint),
      httr::add_headers(Authorization=str_c("Bearer ", api_token()))
    )
  )
}


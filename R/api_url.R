#' Get the base URL of EcoTaxa's API
#'
#' @details The url is "https://ecotaxa.obs-vlfr.fr/api/" by default but can be changed with the option `ecotaxa.url`
#' @export
#' @examples
#' api_url()
#' # set a new default
#' options(ecotaxa.url="https://ecotaxa.somewhere.edu/api/")
#' api_url()
#' # remove the option
#' options(ecotaxa.url=NULL)
#' api_url()
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
#' Fail, with information, if the status code is not 200. Return the parsed response content otherwise.
#'
#' @param x an [httr::response] object.
#' @export
api_handle_response <- function(x) {
  content <- httr::content(x, as="parsed", simplifyVector=TRUE)
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


#' Build a query with parameters
#'
#' @param ... named arguments to be turned into query parameters.
#'
#' @examples
#' query_string(foo="a", bar=NULL, bob=1)
query_string <- function(...) {
  params <- list(...)
  # check
  if (any(names(params) == "")) {
    stop("Query parameters must have names")
  }
  pieces <- sapply(names(params), function(key) {
    val <- params[[key]]
    # TODO deal with vector values
    if (is.null(val)) {
      # null parameters are not in the query
      piece <- NA
    } else {
      # build this piece of the query
      piece <- str_c(key, "=", val)
    }
    return(piece)
  })
  # remove NULL parameters
  pieces <- na.omit(pieces)
  # combine pieces
  query <- str_c("?", str_c(pieces, collapse="&"))
  return(query)
}

## Internal shortcuts to the various http methods  ----

apiGET <- function(endpoint) {
  api_handle_response(
    httr::GET(
      url=str_c(api_url(), endpoint),
      config=httr::config(ssl_verifypeer=FALSE),
      httr::add_headers(Authorization=str_c("Bearer ", api_token()))
    )
  )
}

apiPUT <- function(endpoint, body) {
  api_handle_response(
    httr::PUT(
      url=str_c(api_url(), endpoint),
      config=httr::config(ssl_verifypeer=FALSE),
      body=body, encode="json",
      httr::add_headers(Authorization=str_c("Bearer ", api_token()))
    )
  )
}

apiPOST <- function(endpoint, body) {
  api_handle_response(
    httr::POST(
      url=str_c(api_url(), endpoint),
      config=httr::config(ssl_verifypeer=FALSE),
      body=body, encode="json",
      httr::add_headers(Authorization=str_c("Bearer ", api_token()))
    )
  )
}

apiDELETE <- function(endpoint, body=NULL) {
  api_handle_response(
    httr::DELETE(
      url=str_c(api_url(), endpoint),
      config=httr::config(ssl_verifypeer=FALSE),
      body=body, encode="json",
      httr::add_headers(Authorization=str_c("Bearer ", api_token()))
    )
  )
}

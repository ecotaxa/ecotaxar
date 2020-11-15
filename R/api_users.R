#' List EcoTaxa users
#'
#' This function is accessible to application administrators only.
#'
#' @return A list of users, empty when the current logged in user is not application administrator.
#'
#' @examples
#' api_users()
#'
#' @export
api_users <- function() {
  apiGET("users")
}

#' Get information about the currently authenticated user (i.e. you)
#'
#' @return A list with user properties, including id, email and name.
#'
#' @examples
#' api_users_me()
#'
#' @export
api_users_me <- function() {
  apiGET("users/me")
}

api_users_search <- function(by_name) {
  apiGET(str_c("users/search?by_name=", by_name))
}

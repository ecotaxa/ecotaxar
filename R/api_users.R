#' List EcoTaxa users
#'
#' NB: This function is accessible to application administrators only. Use `[api_users_search()]` as an alternative.
#'
#' @return A list of users or a single user, each containing
#' - `id`: the unique numeric id of this user
#' - `email`: email address, as text
#' - `name`: name, as text
#' - `organisation`: organisation name, as text
#' - `active`: whether the user is still active
#' - `country`: the country name, as text (but chosen in a consistent list)
#' - `usercreationdate`: the date of creation of the user, as text formatted according to the ISO 8601 standard; parse it with `[lubridate::ymd_hms()]`
#' - `usercreationreason`: paragraph describing the usage of EcoTaxa made by the user
#'
#' @export
api_users <- function() {
  apiGET("users")
}

#' Get information about the currently authenticated user (i.e. you)
#'
#' @inherit api_users return
#'
#' @examples
#' api_users_me()
#'
#' @family users
#' @export
api_users_me <- function() {
  apiGET("users/me")
}

#' Search for users, by name
#'
#' @inherit api_users return
#'
#' @examples
#' api_users_search("api")
#'
#' @family users
#' @export
api_users_search <- function(name) {
  apiGET(str_c("users/search?by_name=%", name, "%"))
}

#' Get information for a user, by id
#'
#' @inherit api_users return
#'
#' @examples
#' api_user(993)
#'
#' @family users
#' @export
api_user <- function(id) {
  apiGET(str_c("users/", id))
}


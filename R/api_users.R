#' List EcoTaxa users
#'
#' NB: This function is accessible to application administrators only. Use `[api_users_search()]` as an alternative.
#'
#' @return A list of users or a single user, each containing
#' - `id`: the unique numeric id of this user;
#' - `email`: email address, as text;
#' - `name`: name, as text;
#' - `organisation`: organisation name, as text;
#' - `active`: whether the user is still active;
#' - `country`: the country name, as text (but chosen in a consistent list);
#' - `usercreationdate`: the date of creation of the user, as text formatted according to the ISO 8601 standard; parse it with [lubridate::ymd_hms()];
#' - `usercreationreason`: paragraph describing the usage of EcoTaxa made by the user.
#'
#' @export
api_users <- function() {
  apiGET("users")
}


#' Get information about the currently authenticated user (i.e. you)
#'
#' @inherit api_users return
#'
#' @family users
#' @export
#' @examples
#' api_users_me()
api_users_me <- function() {
  apiGET("users/me")
}


#' Deal with user preferences
#'
#' Get, set, or erase a preference for the currently authenticated user (i.e. you) in a project.
#'
#' @template param-project_id
#' @param key string, the name (key) of the preference. Possible preferences are:
#' - `cwd`: the "current working directory", i.e. last directory on the server from which you imported data in the target project.
#' - `mru`: the list of taxonomic categories "most recently used" in this project. This list is used in the autocompletion of categories in the classification page.
#' @param value the value of the preference, when setting it.
#'
#' @examples
#' # get preference
#' api_get_user_preference(185, "cwd")
#'
#' # set preference and verify it
#' api_get_user_preference(185, key="foo")
#' api_set_user_preference(185, key="foo", value="test")
#' api_get_user_preference(185, key="foo")
#'
#' # delete preference
#' api_del_user_preference(185, key="foo")
#' api_get_user_preference(185, key="foo")
#'
#' @family users
#' @name api_user_preference
NULL

#' @rdname api_user_preference
#' @export
api_get_user_preference <- function(project_id, key) {
  checkmate::assert_string(key)
  apiGET(stringr::str_c("users/my_preferences/", project_id, "?key=", key))
}

#' @rdname api_user_preference
#' @export
api_set_user_preference <- function(project_id, key, value) {
  checkmate::assert_string(key)
  apiPUT(stringr::str_c("users/my_preferences/", project_id, "?key=", key, "&value=", value), body=NULL)
}

#' @rdname api_user_preference
#' @export
api_del_user_preference <- function(project_id, key) {
  checkmate::assert_string(key)
  apiPUT(stringr::str_c("users/my_preferences/", project_id, "?key=", key, "&value="), body=NULL)
}


#' Search for users, by name
#'
#' @param name string to look for in the user name
#'
#' @inherit api_users return
#'
#' @family users
#' @export
#' @examples
#' api_users_search("api")
api_users_search <- function(name) {
  apiGET(str_c("users/search?by_name=%", name, "%"))
}


#' Get information about a single user, by id
#'
#' @param user_id internal, numeric id of the user.
#'
#' @inherit api_users return
#'
#' @family users
#' @export
#' @examples
#' api_user(993)
api_user <- function(user_id) {
  apiGET(str_c("users/", user_id))
}


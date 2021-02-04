#' Filter projects
#'
#' @param title string to look for in the project title.
#' @param instrument string to look for in the project's instrument.
#' @param with_manager_role boolean, when TRUE, show only projects I can manage.
#' @param include_all search all projects, not just the ones I am registered in.
#'
#' @return A list of project objects.
#'
#' @family projects
#' @export
#' @examples
#' # show all my projects
#' api_projects_filter()
#' # show all projects with "Tara" in the title
#' api_projects_filter("tara", include_all=TRUE)
#' # show all projects using the ZooScan
#' api_projects_filter(instrument="zooscan", include_all=TRUE)
api_projects_filter <- function(title=NULL, instrument=NULL, with_manager_role=FALSE, include_all=FALSE) {
  apiGET(
    str_c("projects/search",
      query_string(
        title_filter=title,
        instrument_filter=instrument,
        for_managing=with_manager_role,
        also_others=include_all,
        filter_subset=FALSE
      )
    )
  )
  # TODO check why I am getting a data.frame back
}


#' Get information for a project
#'
#' @template param-project_id
#'
#' @return A list describing the properties of the project.
#'
#' @family projects
#' @export
#' @examples
#' api_project(185)
api_project <- function(project_id) {
  apiGET(str_c("projects/",project_id))
}


#' Update information for a project
#'
#' @template param-project_id
#' @param info a list describing the properties of the project, formatted exactly as the output of api_project().
#'
#' @return NULL upon success.
#'
#' @family projects
#' @export
#' @examples
#' i <- api_project(185)
#' i$comments
#' # change comment and update
#' i$comments <- "This is a test comment"
#' api_project_update(185, i)
#' # check that the comment is changed
#' api_project(185)$comments
#' # change it back to something else
#' i$comments <- "No comment"
#' api_project_update(185, i)
api_project_update <- function(project_id, info) {
    apiPUT(str_c("projects/",project_id), body=info)
}


#' Create a project
#'
#' Create a project and become manager of it. This is reserved to users with the permission to create projects; this is not the case of the default API user, for security reasons.
#'
#' @param title string, the project title.
#' @param visible boolean, when TRUE, the project is visible by all users.
#' @param clone_of_id internal, numeric id of a project to clone as a new one. By default it does not clone anything.
#'
#' @return The numeric id of the newly created project.
#'
#' @family projects
#' @export
#' @examples
#' \dontrun{
#' # log in as a user with the permission to create a project
#' api_login("your@email", "your_password")
#' # create a project
#' project_id <- api_project_create("Test project through the API", visible=FALSE)
#' # get its URL
#' paste0(sub("api", "prj", api_url()), project_id)
#' # list it through the API
#' api_projects_filter("through the API")
#' # then delete it
#' api_project_delete(project_id)
#' api_projects_filter("through the API")
#' }
api_project_create <- function(title, visible=TRUE, clone_of_id=0) {
  # get all function arguments in a list
  body <- as.list(environment())
  apiPOST("projects/create", body=body)
}

#' Delete a project
#'
#' Delete a project (of which you are a manager).
#'
#' @template  param-project_id
#' @param only_objects when TRUE, delete only the objects and keep the project structure (permissions, free column names, etc.) intact.
#'
#' @return ??
#'
#' @family projects
#' @export
#' @examples
#' \dontrun{
#' # log in as a user who is manager on the project to delete
#' api_login("your@email", "your_password")
#' # create a project and delete it
#' project_id <- api_project_create("Test project through the API", visible=FALSE)
#' api_projects_filter("through the API")
#' api_project_delete(project_id)
#' api_projects_filter("through the API")
#' }
api_project_delete <- function(project_id, only_objects=FALSE) {
  apiDELETE(str_c("projects/", project_id, query_string(only_objects=only_objects)))
}


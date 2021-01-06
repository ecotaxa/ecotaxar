#' Batch update metadata of samples
#'
#' @param ids internal ids of the samples to update
#' @param updates named list with the metadata field(s) to update and the updated value(s)
#'
#' @return The number of updated samples.
#' @export
#'
#' @examples
#' # change one field of metadata in one sample
#' api_update_sample_set(15709, list(scan_operator="my test"))
#' # check on the object page for an object in this sample
#' #   https://ecotaxa.obs-vlfr.fr/objectdetails/24473014
#'
#' # change several fields in one sample
#' api_update_sample_set(15709, list(scan_operator="another test", ship="my ship"))
#' api_update_sample_set(15709, list(scan_operator="and another", tow_nb=3))
#'
#' # change several fields in several samples at the same time
#' api_update_sample_set(
#'   c(15709, 15710),
#'   list(scan_operator="the beatles", ship="yellow submarine")
#' )
#' # check on the pages of objects in each of these two samples
#' #   https://ecotaxa.obs-vlfr.fr/objectdetails/24473014
#' #   https://ecotaxa.obs-vlfr.fr/objectdetails/24473015
#'
#' # restore the original values
#' api_update_sample_set(
#'   c(15709, 15710),
#'   list(scan_operator="corinne desnos", ship="sagitta", tow_nb=1)
#' )
api_update_sample_set <- function(ids, updates) {
  body <- list(
    target_ids=as.list(ids),
    updates=format_updates(updates)
  )
  # print(jsonlite::toJSON(body, auto_unbox=T))
  apiPOST("sample_set/update", body=body)
}

# Format updates as a list of named lists for appropriate conversion to JSON
format_updates <- function(x) {
  y <- list()
  n <- names(x)
  for (i in seq_along(x)) {
    y[[i]] <- list("ucol"=n[i], "uval"=x[[i]])
  }
  return(y)
}

#' Batch update metadata of objects
#'
#' @param ids internal ids of the objects to update
#' @param updates named list with the metadata field(s) to update and the updated value(s). Use [api_object()] to find their names. Due to a current [bug](https://github.com/ecotaxa/ecotaxa_dev/issues/556), the list should contain either regular fields only or free fields only, but not a mix of both.
#'
#' @return The number of updated objects.
#'
#' @export
#'
#' @examples
#' # get list of possible fields via the `object` endpoint
#' str(api_object(24473014))
#'
#' # change one field of metadata in one object
#' api_update_object_set(24473014, list(longitude=10))
#' # check that the change is effective
#' api_object(24473014)$longitude
#'
#' # change several fields in one object
#' api_object(24473014)[c("longitude", "latitude")]
#' api_update_object_set(24473014, list(longitude=0, latitude=0))
#' api_object(24473014)[c("longitude", "latitude")]
#'
#' api_object(24473014)$free_columns[c("area", "mean")]
#' api_update_object_set(24473014, list(area=1, mean=1))
#' api_object(24473014)$free_columns[c("area", "mean")]
#'
#' # change several fields in several objects
#' api_update_object_set(c(24473014, 24473015), list(longitude=5, latitude=-2))
#' api_object(24473014)[c("longitude", "latitude")]
#' api_object(24473015)[c("longitude", "latitude")]
#'
#' # change the taxonomic identification
#' api_update_object_set(24473014, list(classif_id=1, classif_qual="V"))
#' api_object(24473014)[c("classif_id", "classif_qual")]
#'
#' # restore initial values
#' api_update_object_set(
#'   c(24473014, 24473015),
#'   list(longitude=7.31567, latitude=43.68500)
#' ) # NB: these objects were actually collected in the same location
#' api_update_object_set(24473014, list(classif_id=11509, classif_qual="P"))
#' api_update_object_set(24473014, list(area=1028, mean=224))
#' api_update_object_set(24473015, list(area=1118, mean=225))
api_update_object_set <- function(ids, updates) {
  # check arguments
  checkmate::check_integerish(ids)
  checkmate::check_list(updates)
  # check that we are not mixing fixed and free fields
  # get the list of fixed fields from the first object
  info <- api_object(ids[1])
  all_fields <- names(info)
  fixed_fields <- setdiff(all_fields, c("images", "free_columns"))
  updates_in_free <- setdiff(names(updates), fixed_fields)
  updates_in_fixed <- intersect(names(updates), fixed_fields)
  if (length(updates_in_fixed) != 0 &  length(updates_in_free) != 0) {
    stop("You are mixing fixed and free fields\n",
         "  fixed: ", stringr::str_c(updates_in_fixed, collapse=", "),"\n",
         "  free: ", stringr::str_c(updates_in_free, collapse=", "),"\n",
         "This is currently impossible. Please perform two separate queries.")
  }

  body <- list(
    target_ids=as.list(ids),
    updates=format_updates(updates)
  )
  # print(jsonlite::toJSON(body, auto_unbox=T))
  apiPOST("object_set/update", body=body)
}

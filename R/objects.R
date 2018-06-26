#' Get objects and associated metadata
#'
#' @param db connection to the database, from \code{\link{src_ecotaxa}}.
#' @param projids numeric identifiers of projects to extract data from.
#' @param ... additional filters on the objects table. Typical filters are on:
#' \itemize{
#' \item \code{classif_qual}: "V" for validated, "P" for predicted, "D" for dubious,
#' \item \code{classif_id}: numeric id of the taxa of interest,
#' \item \code{sampleid}: internal numeric identifier of the sample (from the "samples" table).
#' }
#' @param *_fields vectors of column names to extract in the objects, sample, acquisitions, and process tables. NULL (the default) extracts nothing, "all" extracts all fields.
#'
#' @return A tibble with columns that are systematically extracted and the additional metadata requested
#'
#' @export
#'
#' @examples
#' db <- src_ecotaxa()
#' # extract validated objects from a couple projects with some metadata
#' d <- extract_objects(
#'   db, projids=c(658, 756), classif_qual=="V",
#'   object_fields=c("area", "major"),
#'   process_fields="particle_pixel_size_mm",
#'   acquis_fields="sub_part",
#'   sample_fields="tot_vol"
#' )
#' # extract all objects from a project with all object-level metadata
#' d <- extract_objects(
#'   db, projids=658,
#'   object_fields="all"
#' )
extract_objects <- function(db, projids, ..., object_fields=NULL, process_fields=NULL, acquis_fields=NULL, sample_fields=NULL) {

  # Reduce a mapping to the specified fields
  # @param mapping EcoTaxa mapping string
  # @param fields vector of fields of interest
  # @param title (optional) string containing the project title (for nicer warnings)
  reduce_mapping <- function(proj, fields, level) {
    if (is.null(fields)) {
      map <- NULL
    } else {
      # parse mapping into a named vector
      mapname <- paste0("mapping", switch(level,
        object="obj",
        acquis="acq",
        level
      ))
      map <- parse_mapping(proj[[mapname]])

      # if fields is not the special keyword "all" it is a vector of field names
      # only those fields need to be kept
      if (fields[1] != "all") {
        # detect missing fields and warn about them
        missing <- setdiff(fields, names(map))
        if (length(missing) == 1) {
          warning(
            level, "field ", missing, " is missing",
            " in project '", proj$title, "'",
            ", it will be set to NA.",
            call.=FALSE
          )
        } else if (length(missing) > 1) {
          warning(
            level, "fields ", paste0(missing, collapse=", "), " are missing",
            " in project '", proj$title, "'",
            ", they will be set to NA.",
            call.=FALSE
          )
        }

        # reduce to specified fields
        map <- map[names(map) %in% fields]
      }
    }
    names(map) <- paste0(level, "_", names(map))
    return(map)
  }

  # for each project
  d <- plyr::ldply(projids, function(id) {
    proj <- tbl(db, "projects") %>% filter(projid==id) %>% collect()

    # extract objects
    o <- tbl(db, "objects") %>%
      filter(projid==id, ...) %>%
      # filter(projid==id, classif_qual=="V") %>%
      select(
        # select essential metadata
        projid, sampleid, acquisid, processid, objid, object_orig_id=orig_id,
        object_lat=latitude, object_lon=longitude, object_depth_min=depth_min, object_depth_max=depth_max, object_date=objdate, object_time=objtime,
        object_annotation_id=classif_id, object_annotation_status=classif_qual, object_annotation_person_id=classif_who, object_annotation_datetime=classif_when,
        # select user specified metadata
        reduce_mapping(proj, object_fields, "object")
      ) %>%
      # get info on who classified the object
      left_join(
        select(tbl(db, "users"), id, object_annotation_person_name=name, object_annotation_person_email=email),
        by=c("object_annotation_person_id"="id")
      )

    # get metadata from other levels
    if (!is.null(sample_fields)) {
      o <- left_join(o,
        select(
          tbl(db, "samples"),
          sampleid, reduce_mapping(proj, sample_fields, "sample")
        ),
        by="sampleid"
      )
    }
    if (!is.null(acquis_fields)) {
      o <- left_join(o,
        select(
          tbl(db, "acquisitions"),
          acquisid, reduce_mapping(proj, acquis_fields, "acquis")
        ),
        by="acquisid"
      )
    }
    if (!is.null(process_fields)) {
      o <- left_join(o,
        select(
          tbl(db, "process"),
          processid, reduce_mapping(proj, process_fields, "process")
        ),
        by="processid"
      )
    }

    return(collect(o, n=3))
  }, .progress="text")

  # convert text-as-number columns
  # Convert a vector of strings storing numbers into actual numbers
  # Try to be clever about when to do it
  # @param x input vector
  convert_num <- function(x) {
    # try conversion
    xn <- suppressWarnings(as.numeric(x))
    # if this does not create more NAs as in the original, then accept the conversion
    if (sum(is.na(xn))==sum(is.na(x))) {
      x <- xn
    }
    return(x)
  }
  d <- mutate_at(d, vars(starts_with("process_"), starts_with("acquis_"), starts_with("sample_")), convert_num)
  return(dplyr::as_tibble(d))
}

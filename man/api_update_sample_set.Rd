% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_update.R
\name{api_update_sample_set}
\alias{api_update_sample_set}
\title{Batch update metadata of samples}
\usage{
api_update_sample_set(sample_ids, updates)
}
\arguments{
\item{sample_ids}{internal, numeric ids of the samples.}

\item{updates}{named list containing the metadata field(s) to update and the updated value(s).}
}
\value{
The number of updated samples.
}
\description{
Batch update metadata of samples
}
\examples{
# change one field of metadata in one sample
api_update_sample_set(15709, list(scan_operator="my test"))
# check on the object page for an object in this sample
#   https://ecotaxa.obs-vlfr.fr/objectdetails/24473014

# change several fields in one sample
api_update_sample_set(15709, list(scan_operator="another test", ship="my ship"))
api_update_sample_set(15709, list(scan_operator="and another", tow_nb=3))

# change several fields in several samples at the same time
api_update_sample_set(
  c(15709, 15710),
  list(scan_operator="the beatles", ship="yellow submarine")
)
# check on the pages of objects in each of these two samples
#   https://ecotaxa.obs-vlfr.fr/objectdetails/24473014
#   https://ecotaxa.obs-vlfr.fr/objectdetails/24473015

# restore the original values
api_update_sample_set(
  c(15709, 15710),
  list(scan_operator="corinne desnos", ship="sagitta", tow_nb=1)
)
}

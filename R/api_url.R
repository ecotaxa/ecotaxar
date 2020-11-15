api_url <- function() {
  url <- getOption("ecotaxa.url")
  # TODO look at ways to save this
  if (is.null(url)) {
    url <- "https://ecotaxa.obs-vlfr.fr/api/"
  }
  return(url)
}

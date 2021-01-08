# ecotaxar

An R package to access data from [EcoTaxa](https://ecotaxa.obs-vlfr.fr).

## Installation

The package is not (yet?) on CRAN, so to install:

```
remotes::install_github("ecotaxa/ecotaxar")
```

## Usage

Functions starting with `api_` call [EcoTaxa's API](http://ecotaxa.obs-vlfr.fr/api/docs#/). This is still a work in progress and not all API endpoints are accessible.

Functions starting with `db_` require direct access to the database, which is currently only possible from the internal network of the Laboratoire d'Océanographie de Villefranche (LOV).

A few other convenience functions are provided, to read `.tsv` files output by EcoTaxa and to handle a hierarchical taxonomy.

### =========================================================================
### Thin R wrappers to UCSC REST API endpoints
### -------------------------------------------------------------------------
###
### Nothing in this file is exported.
###


.API_query <- function(endpoint, query=list(), api.url=UCSC.api.url())
{
    stopifnot(isSingleString(endpoint), nzchar(endpoint),
              is.list(query),
              isSingleString(api.url), nzchar(api.url))
    if (length(query) != 0L)
        stopifnot(!is.null(names(query)))
    url <- paste0(api.url, "/", endpoint)
    GET(url, user_agent("Bioconductor UCSC.utils"), query=query)
}

.parse_json <- function(response)
{
    parsed_json <- fromJSON(content(response, as="text", encoding="UTF-8"))
    ## Sanity checks.
    stopifnot(is.list(parsed_json), !is.null(names(parsed_json)))
    parsed_json
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### We only support the following endpoints at the moment:
###   - /list/ucscGenomes
###   - /list/chromosomes
###   - /list/tracks
###   - /getData/track
### See https://genome.ucsc.edu/goldenPath/help/api.html#Endpoint for the
### full list of endpoints.
###
### All the functions below return parsed JSON.
###

### Endpoint /list/ucscGenomes
API_list_genomes <- function(api.url=UCSC.api.url())
{
    endpoint <- "list/ucscGenomes"
    response <- .API_query(endpoint, api.url=api.url)
    if (response[["status_code"]] != 200L)
        stop(wmsg("failed to get list of UCSC genomes from ", api.url))

    ans <- .parse_json(response)[["ucscGenomes"]]
    ## Sanity check.
    stopifnot(is.list(ans))
    ans
}

### Endpoint /list/chromosomes
API_list_chromosomes <- function(genome, api.url=UCSC.api.url())
{
    stopifnot(isSingleString(genome), nzchar(genome))

    endpoint <- "list/chromosomes"
    query <- list(genome=genome)
    response <- .API_query(endpoint, query=query, api.url=api.url)
    if (response[["status_code"]] != 200L)
        stop(wmsg(genome, ": unknown UCSC genome ",
                  "(or ", api.url, " is down?)"))

    ans <- .parse_json(response)
    ## Sanity check.
    stopifnot(identical(ans[["genome"]], genome))
    ans
}

### Endpoint /list/tracks
API_list_tracks <- function(genome, api.url=UCSC.api.url())
{
    stopifnot(isSingleString(genome), nzchar(genome))

    endpoint <- "list/tracks"
    query <- list(genome=genome)
    response <- .API_query(endpoint, query=query, api.url=api.url)
    if (response[["status_code"]] != 200L)
        stop(wmsg(genome, ": unknown UCSC genome ",
                  "(or ", api.url, " is down?)"))

    ans <- .parse_json(response)[[genome]]
    ## Sanity check.
    stopifnot(is.list(ans))
    ans
}

### Endpoint /getData/track
### Note that the endpoint expects the supplied 'track' argument to be the
### name of the track's primary table rather than the track's name.
### E.g. "catLiftOffGenesV1" rather than "CAT/Liftoff Genes".
API_get_track_data <- function(genome, primary_table, api.url=UCSC.api.url())
{
    stopifnot(isSingleString(genome), nzchar(genome),
              isSingleString(primary_table), nzchar(primary_table))

    endpoint <- "getData/track"
    query <- list(genome=genome, track=primary_table)
    response <- .API_query(endpoint, query=query, api.url=api.url)
    if (response[["status_code"]] != 200L)
        stop(wmsg(genome, "/", primary_table, ": ",
                  "unknown UCSC genome/primary_table ",
                  "(or ", api.url, " is down?)"))

    ans <- .parse_json(response)
    ## Sanity checks.
    stopifnot(identical(ans[["genome"]], genome))
    if (!is.null(ans[["track"]]))
        stopifnot(identical(ans[["track"]], primary_table))
    ans
}


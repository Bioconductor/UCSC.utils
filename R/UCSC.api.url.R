### =========================================================================
### UCSC.api.url()
### -------------------------------------------------------------------------
###


UCSC.api.url <- function() getOption("UCSC.api.url")


### Not exported.
query_UCSC_api <- function(endpoint, query=list(), api.url=UCSC.api.url())
{
    stopifnot(isSingleString(endpoint),
              is.list(query),
              isSingleString(api.url))
    if (length(query) != 0L)
        stopifnot(!is.null(names(query)))
    url <- paste0(api.url, "/", endpoint)
    GET(url, query=query)
}


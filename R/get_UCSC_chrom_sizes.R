### =========================================================================
### get_UCSC_chrom_sizes()
### -------------------------------------------------------------------------
### 


### Returns the chromosome sizes in an named numeric vector.
.extract_chrom_sizes_from_JSON <- function(json)
{
    parsed_json <- fromJSON(json)
    stopifnot(is.list(parsed_json), !is.null(names(parsed_json)))
    chrom_count <- parsed_json$chromCount
    stopifnot(isSingleNumber(chrom_count))
    chrom_count <- as.integer(chrom_count)
    chrom_sizes <- parsed_json$chromosomes
    stopifnot(is.list(chrom_sizes),
              !is.null(names(chrom_sizes)),
              length(chrom_sizes) == chrom_count)
    chrom_sizes <- unlist(chrom_sizes, recursive=FALSE)
    stopifnot(is.numeric(chrom_sizes),
              length(chrom_sizes) == chrom_count)
    chrom_sizes
}

get_UCSC_chrom_sizes <- function(genome, api.url=UCSC.api.url(), recache=FALSE)
{
    if (!(isSingleString(genome) && nzchar(genome)))
        stop(wmsg("'genome' must be a single (non-empty) string"))
    if (!isTRUEorFALSE(recache))
        stop(wmsg("'recache' must be TRUE or FALSE"))
    key <- paste0(genome, "_CHROMOSOMES")
    ans <- cached_rest_api_results[[key]]
    if (is.null(ans) || recache) {
        query <- list(genome=genome)
        response <- query_UCSC_api("list/chromosomes", query=query,
                                                       api.url=api.url)
        if (response$status_code != 200L)
            stop(wmsg(genome, ": unknown UCSC genome ",
                      "(or ", response$url, " is down?)"))
        json <- content(response, as="text", encoding="UTF-8")
        ans <- .extract_chrom_sizes_from_JSON(json)
        cached_rest_api_results[[key]] <- ans
    }
    ans
}


### =========================================================================
### list_UCSC_genomes()
### -------------------------------------------------------------------------
###


.get_UCSC_genomes <- function(recache=FALSE)
{
    if (!isTRUEorFALSE(recache))
        stop(wmsg("'recache' must be TRUE or FALSE"))
    ans <- cached_rest_api_results[["ucscGenomes"]]
    if (is.null(ans) || recache) {
        url <- UCSC_REST_API_URL
        response <- GET(url, path="list/ucscGenomes")
        if (response$status_code != 200L)
            stop(wmsg("failed to retrieve list of UCSC genomes"))
        json <- content(response, as="text", encoding="UTF-8")
        ans <- fromJSON(json)[["ucscGenomes"]]
        stopifnot(is.list(ans))  # sanity check
        cached_rest_api_results[["ucscGenomes"]] <- ans
    }
    ans
}

### NOT exported but used in GenomeInfoDb::registered_UCSC_genomes().
### Orders first by 'organism' then by 'genome'.
order_organism_genome_pairs <- function(organism, genome)
{
    regexpr <- "^(.*[^0-9])([0-9]*)$"
    genome_basename <- sub(regexpr, "\\1", genome)
    genome_version <- as.integer(sub(regexpr, "\\2", genome))
    genome_version[is.na(genome_version)] <- 0L
    order(organism, genome_basename, genome_version)
}

### Returns a data.frame with 1 row per genome and 5 columns:
### organism, genome, common_name, tax_id, description.
### A few things align with GenomeInfoDb::registered_UCSC_genomes():
###   - colnames "organism" and "genome" on the returned data frame;
###   - column "organism" returned as a factor;
###   - name, default value, and behavior of 'organism' argument, but with
###     the difference that we also search matches in the "common_name"
###     column (in addition to matches in the "organism" column);
###   - order of rows in the returned data frame.
list_UCSC_genomes <- function(organism=NA, recache=FALSE)
{
    if (!isSingleStringOrNA(organism))
        stop(wmsg("'organism' must be a single string or NA"))
    genomes <- .get_UCSC_genomes(recache=recache)

    ans_organism <- factor(vapply(genomes,
        function(genome) {
            stopifnot(is.list(genome))  # sanity check
            genome$scientificName
        },
        character(1), USE.NAMES=FALSE
    ))
    ans_common_name <- factor(vapply(genomes,
        function(genome) genome$organism,
        character(1), USE.NAMES=FALSE
    ))
    if (!is.na(organism)) {
        keep_idx <- which(grepl(organism, ans_organism, ignore.case=TRUE) |
                          grepl(organism, ans_common_name, ignore.case=TRUE))
        genomes <- genomes[keep_idx]
        ans_organism <- ans_organism[keep_idx]
        ans_common_name <- ans_common_name[keep_idx]
    }

    ans_genome <- names(genomes)
    ans_tax_id <- vapply(genomes,
        function(genome) as.integer(genome$taxId),
        integer(1), USE.NAMES=FALSE
    )
    ans_description <- vapply(genomes,
        function(genome) genome$description,
        character(1), USE.NAMES=FALSE
    )

    ans <- data.frame(
        organism=ans_organism,
        genome=ans_genome,
        common_name=ans_common_name,
        tax_id=ans_tax_id,
        description=ans_description
    )
    oo <- order_organism_genome_pairs(ans$organism, ans$genome)
    S4Vectors:::extract_data_frame_rows(ans, oo)
}

### Convenience helper based on list_UCSC_genomes().
get_organism_for_UCSC_genome <- function(genome)
{
    if (!(isSingleString(genome) && nzchar(genome)))
        stop(wmsg("'genome' must be a single (non-empty) string"))
    df <- list_UCSC_genomes()
    idx <- match(genome, df$genome)
    if (is.na(idx))
        stop(wmsg(genome, ": unknown UCSC genome"))
    as.character(df$organism[idx])
}


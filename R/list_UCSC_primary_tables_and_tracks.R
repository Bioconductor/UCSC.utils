### =========================================================================
### list_UCSC_primary_tables_and_tracks()
### -------------------------------------------------------------------------
###


.get_UCSC_genome_tracks <- function(genome, recache=FALSE)
{
    if (!(isSingleString(genome) && nzchar(genome)))
        stop(wmsg("'genome' must be a single (non-empty) string"))
    if (!isTRUEorFALSE(recache))
        stop(wmsg("'recache' must be TRUE or FALSE"))
    key <- paste0(genome, "_TRACKS")
    ans <- cached_rest_api_results[[key]]
    if (is.null(ans) || recache) {
        url <- UCSC_REST_API_URL
        response <- GET(url, path="list/tracks", query=list(genome=genome))
        if (response$status_code != 200L)
            stop(wmsg(genome, ": unknown UCSC genome (or ", url, " is down?)"))
        json <- content(response, as="text", encoding="UTF-8")
        ans <- fromJSON(json)[[genome]]
        stopifnot(is.list(ans))  # sanity check
        cached_rest_api_results[[key]] <- ans
    }
    ans
}

### Typical usage:
###     list_UCSC_primary_tables_and_tracks("ce2")
###     list_UCSC_primary_tables_and_tracks("mm9", track_group="genes")
###     list_UCSC_primary_tables_and_tracks("hg38", track_group=NA)
### Returns a data.frame with 1 row per primary table and 5 columns:
### primary_table, track, type, track_group, composite_track.
### Note that the "track_group" and "composite_track" columns can contain NAs.
### Passing 'track_group=NA' is accepted and keeps only rows for tracks that
### don't belong to any group. This is why default value for the 'track_group'
### argument is NULL and not NA like for the 'organism' argument in
### list_UCSC_genomes() above.
list_UCSC_primary_tables_and_tracks <-
    function(genome, track_group=NULL, recache=FALSE)
{
    if (!(is.null(track_group) || isSingleStringOrNA(track_group)))
        stop(wmsg("'track_group' must be a single string, or NA, or NULL"))
    genome_tracks <- .get_UCSC_genome_tracks(genome, recache=recache)

    track_groups <- factor(vapply(genome_tracks,
        function(track) {
            stopifnot(is.list(track))  # sanity check
            idx <- match("group", names(track))
            if (is.na(idx)) NA_character_ else track[[idx]]
        },
        character(1), USE.NAMES=FALSE
    ), exclude=character(0))
    if (!is.null(track_group)) {
        keep_idx <- which(track_groups %in% track_group)
        genome_tracks <- genome_tracks[keep_idx]
        track_groups <- track_groups[keep_idx]
    }

    track_names <- vapply(genome_tracks,
        function(track) track$shortLabel,
        character(1), USE.NAMES=FALSE
    )
    track_types <- vapply(genome_tracks,
        function(track) track$type,
        character(1), USE.NAMES=FALSE
    )

    ## Extract tracks nested in composite tracks.
    is_composite <- vapply(genome_tracks,
        function(track) identical(track$compositeTrack, "on"),
        logical(1), USE.NAMES=FALSE
    )
    nested_tracks <- lapply(genome_tracks[is_composite],
        function(track) {
            track[vapply(track, is.list, logical(1), USE.NAMES=FALSE)]
        }
    )
    nested_primary_tables <- lapply(nested_tracks, names)
    nested_track_names <- lapply(nested_tracks,
        function(tracks) vapply(tracks,
                                function(track) track$shortLabel,
                                character(1), USE.NAMES=FALSE)
    )
    nested_track_types <- lapply(nested_tracks,
        function(tracks) vapply(tracks,
                                function(track) track$type,
                                character(1), USE.NAMES=FALSE)
    )
    nested_tracks_count <- lengths(nested_tracks)

    ## Sanity checks.
    stopifnot(
        identical(lengths(nested_primary_tables), nested_tracks_count),
        identical(lengths(nested_track_names), nested_tracks_count),
        identical(lengths(nested_track_types), nested_tracks_count)
    )

    ## Prepare columns of final data frame.
    times <- rep.int(1L, length(genome_tracks))
    times[is_composite] <-  nested_tracks_count
    ans_is_composite <- rep.int(is_composite, times)
    ans_primary_table <- rep.int(names(genome_tracks), times)
    ans_primary_table[ans_is_composite] <-
        unlist(nested_primary_tables, use.names=FALSE)
    stopifnot(anyDuplicated(ans_primary_table) == 0L)  # sanity check
    ans_track <- ans_composite_track <- rep.int(track_names, times)
    ans_track[ans_is_composite] <-
        unlist(nested_track_names, use.names=FALSE)
    ans_type <- rep.int(track_types, times)
    ans_type[ans_is_composite] <-
        unlist(nested_track_types, use.names=FALSE)
    ans_group <- rep.int(track_groups, times)
    ans_composite_track[!ans_is_composite] <- NA_character_

    data.frame(
        primary_table=ans_primary_table,
        track=ans_track,
        type=ans_type,
        track_group=ans_group,
        composite_track=ans_composite_track
    )
}


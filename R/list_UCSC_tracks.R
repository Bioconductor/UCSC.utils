### =========================================================================
### list_UCSC_tracks()
### -------------------------------------------------------------------------
###


.get_UCSC_genome_tracks <- function(genome, api.url=UCSC.api.url(),
                                            recache=FALSE)
{
    check_genome(genome)
    if (!isTRUEorFALSE(recache))
        stop(wmsg("'recache' must be TRUE or FALSE"))
    key <- paste0(genome, "_TRACKS")
    ans <- cached_rest_api_results[[key]]
    if (is.null(ans) || recache) {
        ans <- API_list_tracks(genome, api.url=api.url)
        cached_rest_api_results[[key]] <- ans
    }
    ans
}

### Typical usage:
###     list_UCSC_tracks("ce2")
###     list_UCSC_tracks("mm9", group="genes")
###     list_UCSC_tracks("hg38", group=NA)
### Returns a data.frame with 1 row per track and 5 columns:
### track, primary_table, type, group, composite_track.
### Note that columns "group" and "composite_track" can contain NAs.
### Passing 'group=NA' is accepted and keeps only rows for tracks that
### don't belong to any group. This is why default value for the 'group'
### argument is NULL and not NA like for the 'organism' argument in
### list_UCSC_genomes() above.
list_UCSC_tracks <- function(genome, group=NULL,
                             api.url=UCSC.api.url(), recache=FALSE)
{
    if (!(is.null(group) || isSingleStringOrNA(group)))
        stop(wmsg("'group' must be a single string, or NA, or NULL"))
    genome_tracks <- .get_UCSC_genome_tracks(genome, api.url=api.url,
                                                     recache=recache)

    track_groups <- factor(vapply(genome_tracks,
        function(track) {
            stopifnot(is.list(track))  # sanity check
            idx <- match("group", names(track))
            if (is.na(idx)) NA_character_ else track[[idx]]
        },
        character(1), USE.NAMES=FALSE
    ), exclude=character(0))
    if (!is.null(group)) {
        keep_idx <- which(track_groups %in% group)
        genome_tracks <- genome_tracks[keep_idx]
        track_groups <- track_groups[keep_idx]
    }

    track_shortlabels <- vapply(genome_tracks,
        function(track) track[["shortLabel"]],
        character(1), USE.NAMES=FALSE
    )
    track_types <- vapply(genome_tracks,
        function(track) track[["type"]],
        character(1), USE.NAMES=FALSE
    )

    ## Extract tracks nested in composite tracks.
    is_composite <- vapply(genome_tracks,
        function(track) identical(track[["compositeTrack"]], "on"),
        logical(1), USE.NAMES=FALSE
    )
    nested_tracks <- lapply(genome_tracks[is_composite],
        function(track) {
            track[vapply(track, is.list, logical(1), USE.NAMES=FALSE)]
        }
    )
    nested_track_shortlabels <- lapply(nested_tracks,
        function(tracks) vapply(tracks,
                                function(track) track[["shortLabel"]],
                                character(1), USE.NAMES=FALSE)
    )
    nested_primary_tables <- lapply(nested_tracks, names)
    nested_track_types <- lapply(nested_tracks,
        function(tracks) vapply(tracks,
                                function(track) track[["type"]],
                                character(1), USE.NAMES=FALSE)
    )
    nested_tracks_count <- lengths(nested_tracks)

    ## Sanity checks.
    stopifnot(
        identical(lengths(nested_track_shortlabels), nested_tracks_count),
        identical(lengths(nested_primary_tables), nested_tracks_count),
        identical(lengths(nested_track_types), nested_tracks_count)
    )

    ## Prepare columns of final data frame.
    times <- rep.int(1L, length(genome_tracks))
    times[is_composite] <-  nested_tracks_count
    ans_is_composite <- rep.int(is_composite, times)
    ans_track <- ans_composite_track <- rep.int(track_shortlabels, times)
    ans_track[ans_is_composite] <-
        unlist(nested_track_shortlabels, use.names=FALSE)
    ans_primary_table <- rep.int(names(genome_tracks), times)
    ans_primary_table[ans_is_composite] <-
        unlist(nested_primary_tables, use.names=FALSE)
    stopifnot(anyDuplicated(ans_primary_table) == 0L)  # sanity check
    ans_type <- rep.int(track_types, times)
    ans_type[ans_is_composite] <-
        unlist(nested_track_types, use.names=FALSE)
    ans_group <- rep.int(track_groups, times)
    ans_composite_track[!ans_is_composite] <- NA_character_

    data.frame(
        track=ans_track,
        primary_table=ans_primary_table,
        type=ans_type,
        group=ans_group,
        composite_track=ans_composite_track
    )
}


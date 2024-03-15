.check_list_UCSC_primary_tables_and_tracks_result <- function(df) {
    expect_true(is.data.frame(df))
    EXPECTED_COLNAMES <- c("primary_table", "track", "type",
                           "track_group", "composite_track")
    expect_identical(colnames(df), EXPECTED_COLNAMES)
    expect_true(is.character(df$primary_table))
    expect_true(is.character(df$track))
    expect_true(is.character(df$type))
    expect_true(is.factor(df$track_group))
    expect_true(is.character(df$composite_track))
}

test_that("list_UCSC_primary_tables_and_tracks()", {
    result <- list_UCSC_primary_tables_and_tracks("ce11")
    .check_list_UCSC_primary_tables_and_tracks_result(result)

    result <- list_UCSC_primary_tables_and_tracks("ce11", track_group="genes")
    .check_list_UCSC_primary_tables_and_tracks_result(result)
    expect_true(nrow(result) >= 2L)
    expect_true(all(result$track_group == "genes"))

    result <- list_UCSC_primary_tables_and_tracks("ce11",
                                                  track_group="_not_a_group_")
    .check_list_UCSC_primary_tables_and_tracks_result(result)

    result <- list_UCSC_primary_tables_and_tracks("hg38", track_group=NA)
    .check_list_UCSC_primary_tables_and_tracks_result(result)
    expect_true(nrow(result) >= 2L)
    expect_true(is.na(tail(levels(result$track_group), n=1L)))
    expect_true(all(is.na(as.character(result$track_group))))

    expect_error(list_UCSC_primary_tables_and_tracks(NULL))
    expect_error(list_UCSC_primary_tables_and_tracks(22))
    expect_error(list_UCSC_primary_tables_and_tracks(""))
    expect_error(list_UCSC_primary_tables_and_tracks(NA))
    expect_error(list_UCSC_primary_tables_and_tracks("_not_a_genome_"))
    expect_error(list_UCSC_primary_tables_and_tracks("ce11", track_group=22))
})


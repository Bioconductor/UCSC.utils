.check_list_UCSC_tracks_result <- function(df) {
    expect_true(is.data.frame(df))
    EXPECTED_COLNAMES <- c("track", "primary_table", "type",
                           "group", "composite_track")
    expect_identical(colnames(df), EXPECTED_COLNAMES)
    expect_true(is.character(df$track))
    expect_true(is.character(df$primary_table))
    expect_true(is.character(df$type))
    expect_true(is.factor(df$group))
    expect_true(is.character(df$composite_track))
}

test_that("list_UCSC_tracks()", {
    result <- list_UCSC_tracks("ce11")
    .check_list_UCSC_tracks_result(result)

    result <- list_UCSC_tracks("ce11", group="genes")
    .check_list_UCSC_tracks_result(result)
    expect_true(nrow(result) >= 2L)
    expect_true(all(result$group == "genes"))

    result <- list_UCSC_tracks("ce11", group="_not_a_group_")
    .check_list_UCSC_tracks_result(result)

    result <- list_UCSC_tracks("hg38", group=NA)
    .check_list_UCSC_tracks_result(result)
    expect_true(nrow(result) >= 2L)
    expect_true(is.na(tail(levels(result$group), n=1L)))
    expect_true(all(is.na(as.character(result$group))))

    expect_error(list_UCSC_tracks(NULL))
    expect_error(list_UCSC_tracks(22))
    expect_error(list_UCSC_tracks(""))
    expect_error(list_UCSC_tracks(NA))
    expect_error(list_UCSC_tracks("_not_a_genome_"))
    expect_error(list_UCSC_tracks("ce11", group=22))
})


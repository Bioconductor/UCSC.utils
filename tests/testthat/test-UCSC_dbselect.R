
test_that("UCSC_dbselect()", {
    result <- UCSC_dbselect("gorGor6", "ncbiRefSeqCurated")
    expect_true(is.data.frame(result))
    EXPECTED_COLNAMES <- c("bin", "name", "chrom", "strand",
                           "txStart", "txEnd", "cdsStart", "cdsEnd",
                           "exonCount", "exonStarts", "exonEnds", "score",
                           "name2", "cdsStartStat", "cdsEndStat", "exonFrames")
    expect_identical(colnames(result), EXPECTED_COLNAMES)
    expect_true(nrow(result) >= 100L)

    result <- UCSC_dbselect("gorGor6", "gap")
    expect_true(is.data.frame(result))
    EXPECTED_COLNAMES <- c("bin", "chrom", "chromStart", "chromEnd", "ix" ,                                "n", "size", "type", "bridge")
    expect_identical(colnames(result), EXPECTED_COLNAMES)
    expect_true(nrow(result) >= 100L)

    columns <- c("chrom", "chromStart", "chromEnd", "type")
    where <- "chrom='chrX' AND type='contig'"
    result <- UCSC_dbselect("gorGor6", "gap", columns=columns, where=where)
    expect_true(is.data.frame(result))
    expect_identical(colnames(result), columns)
    expect_true(nrow(result) >= 2L)
})


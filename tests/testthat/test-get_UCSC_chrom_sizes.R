
test_that("get_UCSC_chrom_sizes()", {
    result <- get_UCSC_chrom_sizes("ce11")
    expect_true(is.numeric(result))
    expect_false(is.null(names(result)))

    expect_error(get_UCSC_chrom_sizes(NULL))
    expect_error(get_UCSC_chrom_sizes(22))
    expect_error(get_UCSC_chrom_sizes(""))
    expect_error(get_UCSC_chrom_sizes(NA))
    expect_error(get_UCSC_chrom_sizes("_not_a_genome_"))

})


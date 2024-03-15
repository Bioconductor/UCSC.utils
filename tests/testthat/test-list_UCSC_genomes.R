.check_list_UCSC_genomes_result <- function(df) {
    expect_true(is.data.frame(df))
    EXPECTED_COLNAMES <- c("organism", "genome", "common_name",
                           "tax_id", "description")
    expect_identical(colnames(df), EXPECTED_COLNAMES)
    expect_true(is.factor(df$organism))
    expect_true(is.character(df$genome))
    expect_true(is.factor(df$common_name))
    expect_true(is.integer(df$tax_id))
    expect_true(is.character(df$description))
}

test_that("list_UCSC_genomes()", {
    result <- list_UCSC_genomes("cat")
    .check_list_UCSC_genomes_result(result)
    expect_true(nrow(result) >= 2L)

    result <- list_UCSC_genomes("___XXX___XXX___")
    .check_list_UCSC_genomes_result(result)

    expect_error(list_UCSC_genomes(organism=NULL))
    expect_error(list_UCSC_genomes(organism=22))
})

test_that("get_organism_for_UCSC_genome()", {
    genomes <- c("hs1", "hg38", "dm6", "ce11", "mm39",
                 "dm6", "xenTro10", "mpxvRivers")
    expected <- c("Homo sapiens", "Homo sapiens", "Drosophila melanogaster",
                  "Caenorhabditis elegans", "Mus musculus",
                  "Drosophila melanogaster", "Xenopus tropicalis",
                  "Monkeypox virus")
    names(expected) <- genomes
    result <- get_organism_for_UCSC_genome(genomes)
    expect_identical(result, expected)

    result <- get_organism_for_UCSC_genome(character(0))
    expect_identical(result, setNames(character(0), character(0)))

    expect_error(get_organism_for_UCSC_genome(NULL))
    expect_error(get_organism_for_UCSC_genome(22))
    expect_error(get_organism_for_UCSC_genome(""))
    expect_error(get_organism_for_UCSC_genome(NA))
    expect_error(get_organism_for_UCSC_genome("_not_a_genome_"))
})


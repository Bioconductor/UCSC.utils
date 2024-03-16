### =========================================================================
### fetch_UCSC_track_data()
### -------------------------------------------------------------------------
###


### Quite chockingly, the list-based representation of a table as returned
### by the /getData/track endpoint is row-oriented (i.e. one list element
### per row) instead of column-oriented! Not a very efficient way to
### represent a table in JSON :-/
.make_data_frame_from_list_of_rows <- function(list_of_rows)
{
    stopifnot(is.list(list_of_rows), length(list_of_rows) != 0L)
    ## Turn list of rows into list of columns (transposition).
    ans_colnames <- names(list_of_rows[[1L]])
    list_of_cols <- lapply(setNames(ans_colnames, ans_colnames),
        function(col) {
            sapply(list_of_rows, function(row) row[[col]], USE.NAMES=FALSE)
        }
    )
    as.data.frame(list_of_cols)
}

.extract_table_data_from_parsed_json <- function(parsed_json, primary_table)
{
    stopifnot(is.list(parsed_json), !is.null(names(parsed_json)))
    table_data <- parsed_json[[primary_table]]
    ## The table data is either put all together in a single table or split
    ## into one table per chromosome. If the former then 'table_data' is an
    ## unnamed list with one list element per row in the table. If the latter
    ## then 'table_data' is a named list with one list element per chromosome,
    ## where each list element represents a table.
    stopifnot(is.list(table_data))
    if (is.null(names(table_data))) {
        ## One single table.
        ans <- .make_data_frame_from_list_of_rows(table_data)
    } else {
        ## 'table_data' is a named list with the chromosome names on it. Each
        ## list element in 'table_data' is itself a list that represents a
        ## table.
        dfs <- lapply(table_data[lengths(table_data) != 0L],
                      .make_data_frame_from_list_of_rows)
        ans <- do.call(rbind, unname(dfs))
    }
    stopifnot(nrow(ans) == parsed_json[["itemsReturned"]])
    ans
}

### No caching!
fetch_UCSC_track_data <- function(genome, primary_table, api.url=UCSC.api.url())
{
    check_genome(genome)
    if (!(isSingleString(primary_table) && nzchar(primary_table)))
        stop(wmsg("'primary_table' must be a single (non-empty) string"))
    parsed_json <- API_get_track_data(genome, primary_table, api.url=api.url)
    .extract_table_data_from_parsed_json(parsed_json, primary_table)
}


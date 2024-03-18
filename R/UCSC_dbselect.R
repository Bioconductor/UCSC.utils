### =========================================================================
### UCSC_dbselect()
### -------------------------------------------------------------------------
###


.make_SQL_SELECT <- function(from, columns=NULL, where=NULL)
{
    if (!(isSingleString(from) && nzchar(from)))
        stop(wmsg("'from' must be a single (non-empty) string"))

    if (is.null(columns)) {
        columns <- "*"
    } else {
        if (!is.character(columns))
            stop(wmsg("'columns' must be NULL or a character vector"))
        if (length(columns) == 0L)
            stop(wmsg("'columns' cannot have length zero"))
        if (anyNA(columns) || !all(nzchar(columns)))
            stop(wmsg("'columns' cannot contain NAs or empty strings"))
        columns <- paste0(columns, collapse=",")
    }

    SQL <- sprintf("SELECT %s FROM %s", columns, from)
    if (!is.null(where)) {
        if (!(isSingleString(where) && nzchar(where)))
            stop(wmsg("'where' must be NULL or a single (non-empty) string"))
        SQL <- paste(SQL, "WHERE", where)
    }
    SQL
}

.fix_RMariaDB_blobs <- function(df)
{
    stopifnot(is.data.frame(df))
    idx <- which(vapply(df, inherits, logical(1), "blob"))
    df[idx] <- lapply(df[idx],
        function(col) {
            col2 <- try(toListOfIntegerVectors(col), silent=TRUE)
            if (!inherits(col2, "try-error"))
                return(col2)
            vapply(col, rawToChar, character(1), USE.NAMES=FALSE)
        }
    )
    df
}

.unmangle_RMariaDB_colnames <- function(colnames)
{
    stopifnot(is.character(colnames))
    ncol <- length(colnames)
    if (ncol == 0L)
        return(colnames)

    ## Identify columns with the dumb suffixes.
    dumb_suffixes <- paste0("..", seq_len(ncol))
    nc1 <- nchar(colnames)
    nc2 <- nchar(dumb_suffixes)
    suffixes <- substr(colnames, start=nc1-nc2+1L, stop=nc1)
    idx <- which(suffixes == dumb_suffixes)

    ## Remove the dumb suffixes.
    colnames[idx] <- substr(colnames[idx], start=1L, stop=nc1[idx]-nc2[idx])

    colnames
}

### See https://genome.ucsc.edu/goldenpath/help/mysql.html for how to connect
### to a MariaDB server at UCSC.
### Here is an example of how to query the server on the US west coast from
### the Unix command line:
###
###   mysql --user=genome --host=genome-mysql.soe.ucsc.edu mm10 \
###         -e "select count(*) from knownToLocusLink;"
###
### By default UCSC_dbselect() uses the server located on the US west coast.
UCSC_dbselect <- function(dbname, from, columns=NULL, where=NULL,
                          host="genome-mysql.soe.ucsc.edu",
                          port=3306)
{
    load_package_gracefully("DBI", "UCSC_dbselect()")
    load_package_gracefully("RMariaDB", "UCSC_dbselect()")

    SQL <- .make_SQL_SELECT(from, columns=columns, where=where)
    dbconn <- DBI::dbConnect(RMariaDB::MariaDB(), dbname=dbname,
                                                  username="genome",
                                                  host=host,
                                                  port=port)
    on.exit(DBI::dbDisconnect(dbconn))
    ans <- DBI::dbGetQuery(dbconn, SQL)

    ## Undo some of the unfortunate things RMariaDB does to the result.
    ans <- .fix_RMariaDB_blobs(ans)
    colnames(ans) <- .unmangle_RMariaDB_colnames(colnames(ans))
    ans
}


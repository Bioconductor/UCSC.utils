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
    for (j in seq_along(ans)) {
        if (!inherits(ans[[j]], "blob"))
            next
        ans[[j]] <- toListOfIntegerVectors(ans[[j]])
    }
    ans
}


### =========================================================================
### UCSC_dbselect()
### -------------------------------------------------------------------------
### 


### See https://genome.ucsc.edu/goldenpath/help/mysql.html for how to connect
### to a MySQL server at UCSC.
### Here is an example of how to query the server on the US west coast from
### the Unix command line:
###
###   mysql --user=genome --host=genome-mysql.soe.ucsc.edu mm10 -e "select count(*) from knownToLocusLink;"
###
### By default UCSC_dbselect() uses the server located on the US west coast.
UCSC_dbselect <- function(dbname, from, columns=NULL, where=NULL,
                          server="genome-mysql.soe.ucsc.edu")
{
    load_package_gracefully("DBI", "UCSC_dbselect()")
    load_package_gracefully("RMariaDB", "UCSC_dbselect()")

    columns <- if (is.null(columns)) "*" else paste0(columns, collapse=",")
    SQL <- sprintf("SELECT %s FROM %s", columns, from)
    if (!is.null(where)) {
        stopifnot(isSingleString(where))  # sanity check
        SQL <- paste(SQL, "WHERE", where)
    }
    dbconn <- DBI::dbConnect(RMariaDB::MariaDB(), dbname=dbname,
                                                  username="genome",
                                                  host=server,
                                                  port=3306)
    on.exit(DBI::dbDisconnect(dbconn))
    DBI::dbGetQuery(dbconn, SQL)
}


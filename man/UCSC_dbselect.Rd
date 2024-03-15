\name{UCSC_dbselect}

\alias{UCSC_dbselect}

\title{Conveniently retrieve data from the UCSC MariaDB server}

\description{
  A convenience utility to retrieve data from the UCSC MariaDB server
  via simple \code{SELECT} statements.

  Requires the \pkg{RMariaDB} package!
}

\usage{
UCSC_dbselect(dbname, from, columns=NULL, where=NULL,
              host="genome-mysql.soe.ucsc.edu", port=3306)
}

\arguments{
  \item{dbname}{
    A single string specifying the name of database to connect to.
    This is usually the name of a valid UCSC genome e.g. \code{"hg38"},
    \code{"mm39"}, or \code{"sacCer3"}. See \code{?\link{list_UCSC_genomes}}
    for how to get the list of valid UCSC genome names.
  }
  \item{from}{
    A single string specifying the \emph{input data}, that is, the
    \code{FROM} clause of the \code{SELECT} statement. This is typically
    the name of a table, or a join-clause, or a sub-query.
  }
  \item{columns}{
    A character vector specifying the table columns to retrieve.
  }
  \item{where}{
    A single string specifying the \code{WHERE} clause of the
    \code{SELECT} statement.
  }
  \item{host}{
    A single string specifying the name of the server's host
  }
  \item{port}{
    The TCP/IP port to use to connect to the server.
  }
}

\value{
  A data frame.
}

\seealso{
  \itemize{
    \item \code{\link{list_UCSC_genomes}} to get the list of UCSC genomes.

    \item \url{https://genome.ucsc.edu/goldenpath/help/mysql.html} for
          more information about downloading data from UCSC MariaDB
          databases.
  }
}

\examples{
UCSC_dbselect("hg38", "knownGene", where="chrom='chrM'")

columns <- c("chrom", "chromStart", "chromEnd", "type")
where <- "chrom='chrX' AND type='contig'"
UCSC_dbselect("gorGor6", "gap", columns=columns, where=where)
}

\keyword{manip}

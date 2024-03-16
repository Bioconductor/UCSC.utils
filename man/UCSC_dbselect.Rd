\name{UCSC_dbselect}

\alias{UCSC_dbselect}

\title{Conveniently retrieve data from the UCSC MariaDB server}

\description{
  A convenience utility to retrieve data from the UCSC MariaDB server
  via simple SQL \code{SELECT} statements.

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
    the name of an SQL table, or a join-clause, or a sub-query.
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

\details{
  \code{UCSC_dbselect} is an alternative to \code{\link{fetch_UCSC_track_data}}
  that is more efficient and gives the user more control on what data to
  retrieve exactly from the server.

  However, the downside is that \code{UCSC_dbselect} does not work with all
  tracks! This is because, not all track data are stored in a database:
  some tracks are actually file-based (e.g. bigBed tracks). See below for
  an example.
}

\value{
  A data frame.
}

\seealso{
  \itemize{
    \item \code{\link{list_UCSC_genomes}} to get the list of UCSC genomes.

    \item \code{\link{list_UCSC_tracks}} to get the list of tracks and
          associated primary tables for a given UCSC genome.

    \item \code{\link{fetch_UCSC_track_data}} to fetch the track data for
          a given UCSC genome/track.

    \item \url{https://genome.ucsc.edu/goldenpath/help/mysql.html} for
          more information about downloading data from UCSC MariaDB
          databases.
  }
}

\examples{
### Retrieve primary table of "SGP Genes" track for hg38:
UCSC_dbselect("hg38", "sgpGene", where="chrom='chrM'")

### Retrieve subset of "gap" table for gorGor6:
columns <- c("chrom", "chromStart", "chromEnd", "type")
where <- "chrom='chrX' AND type='contig'"
UCSC_dbselect("gorGor6", "gap", columns=columns, where=where)

## WARNING: UCSC_dbselect() does not work with all tracks!
## For example it does not work with bigBed tracks:
subset(list_UCSC_tracks("hg38", group="genes"), grepl("bigBed 9", type))
\dontrun{
UCSC_dbselect("hg38", "crisprAllTargets")  # table doesn't exist!
}
}

\keyword{manip}

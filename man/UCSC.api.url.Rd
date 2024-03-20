\name{UCSC.api.url}

\alias{UCSC.api.url}

\title{Get or set the default UCSC API URL}

\description{
  Get or set the default UCSC API URL.
}

\usage{
UCSC.api.url(new_url=NULL)
}

\arguments{
  \item{new_url}{
    Alternative UCSC API URL to use by default.
  }
}

\details{
  \code{UCSC.api.url()} is just a convenience wrapper for
  \code{getOption("UCSC.api.url")}.

  To change the default UCSC API URL, call \code{UCSC.api.url(new_url)}
  where \code{new_url} is a single string containing the alternative URL.
  Note that this is just a convenience wrapper for
  \code{options(UCSC.api.url=new_url)}.
}

\value{
  When called with no argument, \code{UCSC.api.url()} returns the URL of
  the UCSC API that is currently used by default.

  When passed a new URL, \code{UCSC.api.url(new_url)} returns the URL of
  the UCSC API that was \emph{previously} used by default. Note that the
  URL is returned invisibly.
}

\seealso{
  \itemize{
    \item \url{https://genome.ucsc.edu/goldenPath/help/api.html#Mirrors}
          a list of alternative UCSC API URLs.

    \item \code{\link{list_UCSC_genomes}} to get the list of UCSC genomes.
  }
}

\examples{
UCSC.api.url()  # current default UCSC API URL
get_UCSC_chrom_sizes("ce11", recache=TRUE)

## Temporarily use the mirror in Asia:
old_url <- UCSC.api.url("https://genome-asia.ucsc.edu/cgi-bin/hubApi")
get_UCSC_chrom_sizes("ce11", recache=TRUE)

## Restore old URL:
UCSC.api.url(old_url)
}

\keyword{manip}

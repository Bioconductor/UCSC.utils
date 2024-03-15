\name{UCSC.api.url}

\alias{UCSC.api.url}

\title{Use an alternative UCSC API URL}

\description{
  Get the default UCSC API URL or change it to an alternative URL.
}

\usage{
UCSC.api.url()
}

\details{
    \code{UCSC.api.url} is just a convenience wrapper for
    \code{getOption("UCSC.api.url")}.

    To use an alternative UCSC API URL, set \code{UCSC.api.url}
    global option to the new URL e.g. with:
\preformatted{options(UCSC.api.url="https://genome-asia.ucsc.edu/cgi-bin/hubApi")}
}

\value{
  The UCSC API URL used by default.
}

\seealso{
  \itemize{
    \item \url{https://genome.ucsc.edu/goldenPath/help/api.html#Mirrors}
          a list of alternative UCSC API URLs.

    \item \code{\link{list_UCSC_genomes}} to get the list of UCSC genomes.
  }
}

\examples{
UCSC.api.url()

## To use the mirror in Asia:
\dontrun{
options(UCSC.api.url="https://genome-asia.ucsc.edu/cgi-bin/hubApi")
}
}

\keyword{manip}

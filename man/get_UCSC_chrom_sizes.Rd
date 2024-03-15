\name{get_UCSC_chrom_sizes}

\alias{get_UCSC_chrom_sizes}

\title{List UCSC chromosome sizes}

\description{
  Get the chromosome sizes of a given UCSC genome.
}

\usage{
get_UCSC_chrom_sizes(genome, api.url=UCSC.api.url(), recache=FALSE)
}

\arguments{
  \item{genome}{
    A single string specifying the name of a UCSC genome e.g. \code{"hs1"},
    \code{"mm39"}, or \code{"sacCer3"}. See \code{?\link{list_UCSC_genomes}}
    for how to get the list of valid UCSC genome names.
  }
  \item{api.url}{
    The URL of the UCSC API. By default, the URL returned by
    \code{UCSC.api.url()} is used. Note that what \code{UCSC.api.url()}
    returns is controlled by a global option. See \code{?\link{UCSC.api.url}}
    for more information.
  }
  \item{recache}{
    \code{get_UCSC_chrom_sizes()} uses a cache mechanism so the information
    retrieved for a given genome only gets downloaded once during the
    current R session (note that the caching is done in memory so
    cached information does NOT persist across sessions).
    Setting \code{recache} to \code{TRUE} forces a new download (and
    recaching) of the chromosome sizes for the specified genome.
  }
}

\value{
  A named numeric vector. The names on the vector are the UCSC
  chromosomes/sequences. The vector values are the corresponding lengths.

  Note that the vector is not sorted in any particular order. In particular
  there's not guarantee that the chromosomes will precede the scaffolds.
}

\seealso{
  \itemize{
    \item \code{\link{list_UCSC_genomes}} to get the list of UCSC genomes.

    \item The \code{\link[GenomeInfoDb]{Seqinfo}} constructor function
          in the \pkg{GenomeInfoDb} package for an alternate (higher level)
          way of retrieving the chromosome information of a given NCBI
          assembly or UCSC genome.

    \item \code{\link{list_UCSC_primary_tables_and_tracks}} to get the
          list of primary tables and associated tracks for a given
          UCSC genome.

    \item \code{\link{UCSC.api.url}} for how to use an alternative
          UCSC API URL by default.
  }
}

\examples{
get_UCSC_chrom_sizes("ce2")

get_UCSC_chrom_sizes("hg38")
}

\keyword{manip}

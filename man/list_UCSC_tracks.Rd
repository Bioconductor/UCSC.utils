\name{list_UCSC_tracks}

\alias{list_UCSC_tracks}

\title{List UCSC tracks and associated primary tables}

\description{
  Get the list of tracks and associated primary tables for a given
  UCSC genome.
}

\usage{
list_UCSC_tracks(genome, group=NULL,
                 api.url=UCSC.api.url(), recache=FALSE)
}

\arguments{
  \item{genome}{
    A single string specifying the name of a UCSC genome e.g. \code{"hg38"},
    \code{"mm39"}, or \code{"sacCer3"}. See \code{?\link{list_UCSC_genomes}}
    for how to get the list of valid UCSC genome names.
  }
  \item{group}{
    \code{NULL} or a single string specifying the group of tracks
    to return. By default, all tracks are returned.
    Passing \code{group=NA} is accepted and will return only rows
    associated with tracks that don't belong to any group.
  }
  \item{api.url}{
    The URL of the UCSC API. By default, the URL returned by
    \code{UCSC.api.url()} is used. Note that what \code{UCSC.api.url()}
    returns is controlled by a global option. See \code{?\link{UCSC.api.url}}
    for more information.
  }
  \item{recache}{
    \code{list_UCSC_tracks()} uses a cache mechanism
    so the information retrieved for a given genome only gets downloaded
    once during the current R session (note that the caching is done in
    memory so cached information does NOT persist across sessions).
    Setting \code{recache} to \code{TRUE} forces a new download (and
    recaching) of the list of tracks for the specified genome.
  }
}

\value{
  A data frame with 1 row per track and 5 columns:
  \code{track}, \code{primary_table}, \code{type}, code{group},
  \code{composite_track}.

  Note that columns \code{group} and \code{composite_track} can
  contain \code{NA}s.
}

\seealso{
  \itemize{
    \item \code{\link{list_UCSC_genomes}} to get the list of UCSC genomes.

    \item \code{\link{fetch_UCSC_track_data}} to fetch the track data for
          a given UCSC genome/track.

    \item \code{\link{get_UCSC_chrom_sizes}} to get the chromosome sizes
          of a given UCSC genome.

    \item \code{\link{UCSC.api.url}} for how to use an alternative
          UCSC API URL by default.
  }
}

\examples{
## List all tracks for ce2 genome:
list_UCSC_tracks("ce2")

## List tracks in the "rna" group only:
list_UCSC_tracks("ce2", group="rna")

## Note that some tracks don't belong to any group:
list_UCSC_tracks("hg38", group=NA)
}

\keyword{manip}

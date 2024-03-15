\name{list_UCSC_genomes}

\alias{list_UCSC_genomes}
\alias{get_organism_for_UCSC_genome}

\title{List UCSC genomes}

\description{
  Get the list of UCSC genomes.
}

\usage{
list_UCSC_genomes(organism=NA, recache=FALSE)

get_organism_for_UCSC_genome(genome, recache=FALSE)
}

\arguments{
  \item{organism}{
    By default all UCSC genomes are returned (in a data frame).
    When \code{organism} is specified, \code{list_UCSC_genomes()}
    will only return the rows associated with the specified organism.
    \code{organism} must be supplied as a single string that will be
    used to perform a search (with \code{grep()}) on the \code{organism}
    and \code{common_name} columns of the data frame to return.
    The search is case-insensitive.
  }
  \item{recache}{
    \code{list_UCSC_genomes()} uses a cache mechanism so the list of genomes
    only gets downloaded once during the current R session (note that the
    caching is done in memory so cached information does NOT persist across
    sessions).
    Setting \code{recache} to \code{TRUE} forces a new download (and
    recaching) of the list of genomes.

    In the case of \code{get_organism_for_UCSC_genome()}, the supplied
    \code{recache} value is just passed down to the internal call
    to \code{list_UCSC_genomes()}.
  }
  \item{genome}{
    A character vector of valid UCSC genomes e.g. \code{"hg38"},
    \code{"mm39"}, or \code{"sacCer3"}.
  }
}

\value{
  For \code{list_UCSC_genomes}: A data frame with 1 row per genome
  and 5 columns: \code{organism}, \code{genome}, \code{common_name},
  \code{tax_id}, \code{description}.

  For \code{get_organism_for_UCSC_genome}: A named character of the same
  length as the input containing the scientific names of the organisms
  associated with the supplied UCSC genomes.
}

\seealso{
  \itemize{
    \item \url{https://genome.ucsc.edu/FAQ/FAQreleases.html} for the online
          HTML page that lists all UCSC genome releases, including archived
          ones.

    \item \code{\link[GenomeInfoDb]{registered_UCSC_genomes}} in the
          \pkg{GenomeInfoDb} package for a similar function that returns
          only UCSC genomes registered in the \pkg{GenomeInfoDb} package.

    \item \code{\link{get_UCSC_chrom_sizes}} to get the chromosome sizes
          of a given UCSC genome.

    \item \code{\link{list_UCSC_primary_tables_and_tracks}} to get the
          list of primary tables and associated tracks for a given
          UCSC genome.
  }
}

\examples{
list_UCSC_genomes("human")

list_UCSC_genomes("pacos")

get_organism_for_UCSC_genome(c("ce11", "xenTro10", "mpxvRivers"))
}

\keyword{manip}

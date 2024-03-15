.onLoad <- function(libname, pkgname)
{
    ## Alternative UCSC API URLs:
    ## - Europe: https://genome-euro.ucsc.edu/cgi-bin/hubApi
    ## - Asia: https://genome-asia.ucsc.edu/cgi-bin/hubApi
    ## - Mirror installation: https://your.server.edu/cgi-bin/hubApi
    url <- "https://api.genome.ucsc.edu"  # primary URL (West Coast)
    options(UCSC.api.url=url)
}


### =========================================================================
### UCSC.api.url()
### -------------------------------------------------------------------------
###


UCSC.api.url <- function(new_url=NULL)
{
    ans <- getOption("UCSC.api.url")
    if (is.null(new_url))
        return(ans)
    if (!(isSingleString(new_url) && nzchar(new_url)))
        stop(wmsg("'new_url' must be a single (non-empty) string"))
    if (!startsWith(tolower(new_url), "http"))
        stop(wmsg("'new_url' must start with \"http\""))
    options(UCSC.api.url=new_url)
    invisible(ans)  # return old URL invisibly
}


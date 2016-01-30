exhaust_search <- function(FUN = searchFunction, PARSER = parseFunction ,...)
{
  results <- list()
  curPage <- 1
  keepSearching <- TRUE
  while(keepSearching == TRUE)
  {
    curResult <- PARSER(FUN(..., pageNumber= curPage))
    if(!is.null(curResult))
    {
      results <- c(results, list(curResult))
      curPage <- curPage + 1
    } else
    {
      keepSearching <- FALSE
    }
  }
  unlist(results, recursive = F)
}

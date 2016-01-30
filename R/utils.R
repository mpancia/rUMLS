exhaust_search <- function(FUN = searchFunction, ...)
{
  results <- list()
  curPage <- 1
  keepSearching <- TRUE
  while(keepSearching == TRUE)
  {
    curResult <- parse_search(FUN(..., pageNumber= curPage))
    print(curResult)
    if(!is.null(curResult))
    {
      results <- c(results, curResult)
      curPage <- curPage + 1
    } else
    {
      keepSearching <- FALSE
    }
  }
  unlist(results)
}

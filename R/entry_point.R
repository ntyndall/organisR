#' @title Entry Point
#'
#' @export


entry_point <- function(entryPoint, contents, functionNames) {
  toInvestigate <- entryPoint
  totalFuns <- currentFuns <- c()
  keepGoing <- TRUE

  # Loop through all functions and check the content
  while (keepGoing) {
    toInvestigate %<>% paste0(" <- function")
    # Loop over all functions to investigate
    for (i in 1:(toInvestigate %>% length)) {

      # Get file details
      content <- contents %>%
        organisR::get_fun_contents(
          inv = toInvestigate[i]
        )

      # Get function calls from content
      fstack <- content %>%
        organisR::fun_calls(
          functionNames = functionNames
        )

      # Append to total
      totalFuns %<>% c(fstack)
      currentFuns %<>% c(fstack)
    }

    # Reset items
    if (currentFuns %>% length %>% `>`(0)) {
      toInvestigate <- currentFuns
      currentFuns <- c()
    } else {
      keepGoing <- FALSE
    }
  }

  # Check for functions that aren't called from this entry point
  #neverUsed <- functionNames %>% setdiff(c(totalFuns %>% unique, entryPoint))

  # Return the function counts back
  return(totalFuns)
}

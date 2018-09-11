#' @title Entry Point
#'
#' @export


entry_point <- function(entryPoint, contents, functionNames) {
  toInvestigate <- entryPoint
  totalFuns <- currentFuns <- c()
  keepGoing <- TRUE

  # Loop through all functions and check the content
  while (keepGoing) {

    # Loop over all functions to investigate
    for (i in 1:(toInvestigate %>% length)) {

      # Check to see if it is a function or a script
      funOrScript <- toInvestigate[i] %>%
        grepl(pattern = "/") %>%
        `!`()

      # Look for function
      if (funOrScript) toInvestigate[i] %<>% paste0(" <- function")

      # Get file details
      content <- if (funOrScript) {
        content <- contents %>%
          organisR::get_fun_contents(
            inv = toInvestigate[i]
          )
      } else {
        # Just read the contents of this script...
        readLines(getwd() %>% paste0("/", toInvestigate[i], ".R"))
      }

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
      funOrScript <- TRUE
    } else {
      keepGoing <- FALSE
    }
  }

  # Check for functions that aren't called from this entry point
  #neverUsed <- functionNames %>% setdiff(c(totalFuns %>% unique, entryPoint))

  # Return the function counts back
  return(totalFuns)
}

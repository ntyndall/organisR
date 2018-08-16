#' @title All Used
#'
#' @export


all_used <- function(contents, functionNames) {

  # Create an initial list
  fCounts <- 0 %>%
    rep(functionNames %>% length) %>%
    as.list
  names(fCounts) <- functionNames

  # Go through the contents again and find actual function calls (-1 for actual function)

  # Main names
  functionNames2 <- functionNames %>% paste0(" <- function")

  for (x in 1:(contents %>% length)) {
    res <- sapply(
      X = 1:(functionNames %>% length),
      FUN = function(y) {
        matched <- contents[[x]] %>%
          grepl(pattern = functionNames[y])
        # Total sum
        totSum <- matched %>% sum
        if (matched %>% `&`(contents[[x]] %>% grepl(pattern = functionNames2[y])) %>% any) {
          totSum %>% `-`(1)
        } else {
          totSum
        }
      }
    )

    # Now add the results to the
    fMatches <- res %>%
      `!=`(0)

    if (fMatches %>% any) {
      toAdd <- functionNames %>% `[`(fMatches)
      toAddCounts <- res %>% `[`(fMatches)
      for (k in 1:(toAdd %>% length)) fCounts[[toAdd[k]]] %<>% `+`(toAddCounts[k])
    }
  }

  # Print out the results
  # ...
}

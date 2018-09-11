#' @title Dead
#'
#' @export
#'
#' @importFrom magrittr %<>% %>%


dead <- function(entryPoints = "") {

  # Define the directory
  dir <- getwd() %>%
    paste0("/R/")

  # Get all the files
  allFiles <- dir %>%
    paste0(dir %>% list.files)

  # Get all function contents
  contents <- lapply(
    X = allFiles,
    FUN = function(x) x %>% readLines
  )

  # Sanatize the entryPoints here
  entryPoints %<>%
    organisR::check_wildcards()

  # Loop over every file and take the package function names
  functionNames <- contents %>%
    organisR::get_fun_names()

  # Look through everything, or just at an entry point
  results <- if (entryPoints[1] == "") {
    contents %>%
      organisR::all_used(
        functionNames = functionNames
      ) %>%
      list
  } else {
    lapply(
      X = entryPoints,
      FUN = function(x) {
        x %>% organisR::entry_point(
          contents = contents,
          functionNames = functionNames
        )
      }
    )
  }

  # Print header
  cat(crayon::blue(paste0(" Looking at function calls from entry points -- ", entryPoints  %>% paste(collapse = " / "))), "\n\n ")

  # Combine to get total counts
  total <- results %>%
    purrr::flatten_chr() %>%
    table %>%
    sort(decreasing = TRUE) %>%
    as.list

  allNames <- total %>% names
  # Loop over each and count within each entry point
  for (i in 1:(total %>% length)) {
    # Get counts per entry point
    eCounts <- results %>%
      purrr::map(function(x) x %>% `==`(allNames[i])) %>%
      purrr::map(sum) %>%
      purrr::flatten_dbl()

    # Don't report on those that are zero
    zeros <- eCounts < 1
    if (zeros %>% any) {
      currentEntries <- entryPoints %>% `[`(!zeros)
      cScores <- eCounts %>% `[`(!zeros)
    } else {
      currentEntries <- entryPoints
      cScores <- eCounts
    }

    # Transform into a percentage
    pers <- cScores %>%
      `/`(total[[i]]) %>%
      scales::percent()

    # , collapse the different entry points
    components <- paste0(currentEntries, " : ", pers) %>%
      paste(collapse = ", ")

    # Cat out result
    cat(crayon::green(paste0(allNames[i], " -- ", total[[i]], " { ", components, " } \n ")))
  }

  # Now report on dead code
  cat("\n")
  cat(crayon::blue("\n Possible dead code ... \n\n "))

  # Check to see which functions are never called...
  neverUsed <- functionNames %>% setdiff(allNames)

  # Collapse and print out
  if (neverUsed %>% length %>% `>`(0)) {
    cat(crayon::red(paste0(neverUsed, " : ", 0, "\n")))
  }
}

#' @title Dead
#'
#' @export
#'
#' @importFrom magrittr %<>% %>%


dead <- function(entryPoint = "") {

  # Define the directory
  dir <- getwd() %>%
    paste0("/R/")

  # Get all the files
  allFiles <- dir %>% paste0(dir %>% list.files)

  # Get all function contents
  contents <- lapply(
    X = allFiles,
    FUN = function(x) x %>% readLines
  )

  # Loop over every file and take the package function names
  functionNames <- contents %>%
    organisR::get_fun_names()

  # Look through everything, or just at an entry point
  if (entryPoint == "") {
    contents %>%
      organisR::all_used(
        functionNames = functionNames
      )
  } else {
    for (i in 1:(entryPoint %>% length)) {
      entryPoint[i] %>% organisR::entry_point(
        contents = contents,
        functionNames = functionNames
      )
    }
  }
}

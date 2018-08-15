#' @title Dead
#'
#' @export


dead <- function() {

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
  functionNames <- lapply(
    X = 1:(contents %>% length),
    FUN = function(x) {
      fs <- contents[[x]] %>%
        `[`(contents[[x]] %>% grepl(pattern = " <- function")) %>%
        strsplit(split = " <- function") %>%
        purrr::map(1) %>%
        purrr::flatten_chr()
      fs %>% `[`(fs %>% grepl(pattern = " ") %>% `!`())
    }
  ) %>%
    purrr::flatten_chr()

  # Now see which functions are used internally
}

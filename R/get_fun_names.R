#' @title Get Function Names
#'
#' @export


get_fun_names <- function(contents) {
  return(
    lapply(
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
  )
}

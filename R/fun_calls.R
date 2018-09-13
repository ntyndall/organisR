#' @title Function Calls
#'
#' @export


fun_calls <- function(curr, fns) {
  return(
    sapply(
      X = 1:(curr %>% length),
      FUN = function(x) {
        sapply(
          X = fns,
          FUN = function(y) {
            if (curr[x] %>% grepl(pattern = paste0(y, "(\\(|\\)| )"))) y else c()
          }
        ) %>%
          purrr::flatten_chr()
      }
    ) %>%
      purrr::flatten_chr()
  )
}

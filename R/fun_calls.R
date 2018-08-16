#' @title Function Calls
#'
#' @export


fun_calls <- function(curr, functionNames) {
  return(
    sapply(
      X = 1:(curr %>% length),
      FUN = function(x) {
        sapply(
          X = 1:(functionNames %>% length),
          FUN = function(y) {
            match <- curr[x] %>% grepl(pattern = paste0(functionNames[y], "(\\(|\\)| )"))
            if (match) {
              functionNames[y]
            } else {
              c()
            }
          }
        ) %>%
          purrr::flatten_chr()
      }
    ) %>%
      purrr::flatten_chr()
  )
}

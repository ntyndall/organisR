#' @title Get Function Contents
#'
#' @export


get_fun_contents <- function(contents, inv) {
  # Which file is it int
  ind <- sapply(
    X = 1:(contents %>% length),
    FUN = function(x) {
      contents[[x]] %>% startsWith(inv) %>% any
    }
  ) %>%
    which

  # Function start
  fStart <- contents[[ind]] %>%
    grepl(pattern = inv) %>%
    which

  # Function endings
  fEndings <- contents[[ind]] %>%
    strsplit(split = "") %>%
    purrr::map(1) %>%
    lapply(function(x) if (x %>% is.null) " " else x) %>%
    purrr::flatten_chr() %>%
    `==`("}") %>%
    which

  fEnd <- fEndings %>%
    `[`(fStart %>% `<`(fEndings) %>% which %>% min)

  # Return just that functions details
  return(contents[[ind]][(fStart + 1):fEnd])

}

#' @title Order Tags
#'
#' @export


order_tags <- function(gtags) {

  # Remove any tags containing characters
  gtags %<>% gsub(
    pattern = '[[:alpha:]]',
    replacement = ''
  )

  # Get . split versions
  taglen <- gtags %>%
    strsplit(split = "[.]") %>%
    purrr::map(length) %>%
    purrr::flatten_dbl()

  # Get the max depth, major.minor. ... etc
  maxDepth <- splittags %>%
    purrr::map(length) %>%
    purrr::flatten_dbl() %>%
    max

  # Extend to a consistent length
  extend <- taglen %>% `!=`(maxDepth)
  if (extend %>% any) {
    extend %<>% which
    for (i in extend) {
      gtags[i] %<>% paste0(
        ".0" %>% rep(maxDepth - taglen[i])
      )
    }
  }

  # Get a number representation
  numbered <- gtags %>%
    strsplit(split = "[.]") %>%
    purrr::map(function(x) x %>% as.double %>% `+`(1) %>% paste0(collapse = "")) %>%
    purrr::flatten_chr() %>%
    as.double

  # Now order the tags
  gtags %<>% `[`(numbered %>% order %>% rev)

  # Return the tags back
  return(gtags)
}

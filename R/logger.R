#' @title Logger
#'
#' @export


logger <- function(..., color = "none", m = TRUE) {
  # Convert the text
  txt <- list(...) %>%
    purrr::flatten_chr() %>%
    paste(collapse = " ")

  # Define any colouring
  myfun <- tryCatch(
    expr = getFromNamespace(x = color, ns = "crayon"),
    error = function(e) NULL
  )

  # Print out header / middle
  cat(
    if (m) {
      paste0(Sys.time(), " | ")
    } else {
      "" %>% rep(21) %>% paste(collapse = " ") %>% paste0(". ")
    }
  )

  # Print out the message to screen
  cat(if (myfun %>% is.null) txt else myfun(txt))
  cat("\n")
}

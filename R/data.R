#' @title Data
#'
#' @export

data <- function(pkg = ".") {

  # Read the correct package
  pkg <- if (pkg == ".") {
    getwd() %>%
      strsplit(split = "/") %>%
      purrr::flatten_chr() %>%
      utils::tail(1)
  } else {
    pkg
  }

  # Try and load the package
  loaded <- require(pkg, character.only = TRUE)
  if (!loaded) stop(" ## Package is not installed, try installing first")

  # Check what data sets are available
  myData <- utils::data(package = pkg) %>%
    `[[`("results")

  # Get all the training data set names
  allDataNames <- myData[ , 'Item']
  dLen <- allDataNames %>% length
  allTitles <- myData[ , 'Title']

  prnt_head <- function(...) paste0("  ", crayon::bgBlack(paste(..., collapse = "")), "\n")

  cat("\n")
  cat(prnt_head("## Available data sets"))
  cat(prnt_head("## For newly created data sets you may need to document and rebuild with devtools!"))
  cat("\n")

  # Create full data set here
  dataList <- c()
  if (dLen %>% `>`(0)) {
    dataList %<>% c(prnt_head("## Summary:", dLen, "data sets found."))
    for (i in 1:dLen) {
      # See if any documentation exists
      txt <- tryCatch(
        expr = gbRd::Rd_help2txt(rdName, pkgname = package),
        error = function(e) NULL
      )

      # Chose the correct symbol
      symbol <- if (txt %>% is.null) {
        crayon::red(clisymbols::symbol$cross)
      } else {
        crayon::green(clisymbols::symbol$tick)
      }

      # Append text on
      dataList %<>% c(
        paste0(
          " | ", clisymbols::symbol$circle_filled, '  ', allDataNames[i] , '\n',
          "  |     ", clisymbols::symbol$arrow_right, '  Docs : ', symbol, '\n',
          "  |     ", clisymbols::symbol$arrow_right, '  Title : `', allTitles[i], '`\n'
        )
      )
    }
  } else {
    dataList %<>% c("0 data sets found.")
  }
  cat(dataList)
  cat(prnt_head("## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*"))
  cat("\n\n")

  # Print further documentation for info on data set
  investigate <- TRUE
  titlePattern <- "_\b"
  while (investigate) {
    # Print header
    cat(prnt_head("## Allowed data names -"))

    # Print all data sets
    cat('  ', clisymbols::symbol$circle_filled, '',
        allDataNames %>% paste(collapse = paste0('  \n   ', clisymbols::symbol$circle_filled, '  ')))

    # Ask user for input
    cat("\n\n")
    cat(prnt_head("## Choose a data set, i.e. `", allDataNames[1], "` (or type q to quit)"))
    cat(prnt_head("## Data set :"))
    cat("  ")
    newTxt <- c()
    dataSet <- readLines(con = "stdin", n = 1)

    # Check if the data input is in the allowed names
    if (dataSet %in% allDataNames) {
      # See if the documentation exists...
      helpTxt  <- tryCatch(
        expr = gbRd::Rd_help2txt(dataSet, pkgname = pkg),
        error = function(e) NULL
      )

      if (`!`(helpTxt %>% is.null)) {
        for (j in 1:(helpTxt %>% length)) {
          title <- helpTxt[j] %>% grepl(pattern = titlePattern)
          if (title) {
            newTxt %<>% c(
              helpTxt[j] %>%
                strsplit(split = titlePattern) %>%
                purrr::flatten_chr() %>%
                paste(collapse = '') %>%
                paste0('\n')
            )
          } else {
            newTxt %<>% c(paste0(helpTxt[j], '\n'))
          }
        }
      } else {
        cat(crayon::red(paste0("  ## ", dataSet, " has no documentation... \n")))
      }
    } else {
      if (dataSet %>% `==`('q')) {
        investigate <- FALSE
      } else {
        cat(crayon::red(paste0("  ## ", dataSet, " does not exist... \n")))
      }
    }

    # Print out results of gbRd
    cat(newTxt)
  }

  cat('\n  ## Complete.\n')
}

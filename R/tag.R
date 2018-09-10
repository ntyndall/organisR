#' @title Tag
#'
#' @export


tag <- function(binLoc = "tagged/") {
  # Set up working directory
  dir <- getwd()

  # Load the DESCRIPTION YAML file
  descFile <- dir %>%
    paste0("/DESCRIPTION") %>%
    yaml::yaml.load_file()

  descFileNames <- names(descFile)
  if (descFileNames %>% purrr::has_element("Version")) {
    ver <- descFile[["Version"]]
  } else {
    cat(crayon::red(" ## Cannot find a valid _Version_ key in the DESCRIPTION file --"))
    stop(" ## STOPPING --")
  }

  # Find the current tags from github
  currentTag <- "git tag" %>%
    system(intern = TRUE) %>%
    order_tags() %>%
    `[`(1)

  # Split version in description
  verSplit <- ver %>%
    strsplit(split = "[.]") %>%
    purrr::flatten_chr()

  # Split current git tag
  currentSplit <- currentTag %>%
    strsplit(split = "[.]") %>%
    purrr::flatten_chr()

  # Get their respective lengths
  verLen <- verSplit %>% length
  currentLen <- currentSplit %>% length
  diffLen <- verLen %>%
    `-`(currentLen) %>%
    abs

  # Align the lengths if they are off by major . minor . path versioning style
  if (diffLen != 0) {
    appendVals <- "0" %>% rep(diffLen %>% abs)
    if (verLen > currentLen) currentSplit %<>% c(appendVals) else verSplit %<>% c(appendVals)
  }

  # Compare the two once they have been cleaned up
  compare <- utils::compareVersion(
    a = paste(verSplit, collapse = '.'),
    b = paste(currentSplit, collapse = '.')
  )

  # Print to screen what is about to happen
  if (compare == 0) {
    cat(crayon::yellow(" ## Version has not changed between current tag and DESCRIPTION file --"))
    stop(" ## STOPPING -- ")
  } else if (compare < 0) {
    cat(crayon::yellow(" ## Version is somehow lower in the DESCRIPTION file --"))
    stop(" ## STOPPING -- ")
  }

  # Once versioning has been approved by the checks then create the tar and move it to /tagged/
  devtools::build(
    pkg = dir,
    binary = TRUE
  )

  # ... find the built tar
  latestRelease <- list.files(
    path = "../",
    pattern = "*.tar.gz"
  )

  # Make sure tagged exists
  if (binLoc %>% dir.exists %>% `!`()) binLoc %>% dir.create

  # ... rename file to latest release and move to /tagged/
  renameSuccess <- file.rename(
    from = paste0("../", latestRelease),
    to = paste0(binLoc, latestRelease)
  )

  # If successful then print to screen
  if (renameSuccess) {
    cat(
      crayon::green(
        " ## Tagged latest release successfully from [", currentTag, "] to [", ver, "] -- \n",
        "## Double check the versions are as required and complete the tagging on github! -- "
      )
    )
  } else {
    cat(
      crayon::red(
        " ## Warning! Could not tag release : Manually remove .tar file -- "
      )
    )
  }

  # Newline
  cat("\n")
}

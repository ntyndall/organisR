#' @title Check Wildcards
#'
#' @export


check_wildcards <- function(entryPoints) {
  # Sanatize the entryPoints here
  wildcards <- entryPoints %>%
    grepl(pattern = "[*]")

  if (wildcards %>% any) {
    allfolders <- entryPoints %>%
      `[`(wildcards) %>%
      gsub(pattern = "[[:punct:]]", replacement = "")

    # Remove any wildcard locations
    entryPoints %<>% `[`(!wildcards)

    # Check for all the files contained
    subfiles <- lapply(
      X = allfolders,
      FUN = function(x) {
        if (x %>% dir.exists) {
          allfiles <- list.files(getwd() %>% paste0("/", x)) %>%
            gsub(pattern = "[.R]", replacement = "")
          paste0(allfolders, "/", allfiles)
        } else {
          NULL
        }
      }
    ) %>%
      purrr::flatten_chr()

    # Append the rest on
    entryPoints %<>% c(subfiles)
  }

  # Return possibly altered entryPoints
  return(entryPoints)
}

#' @title Test Output
#'
#' @export


test_output <- function(results, logs = TRUE) {
  totalError <- 0
  for(i in 1:length(results)) {
    singleTest <- results[i][[1]]
    testResults <- singleTest$results
    failures <- 0

    for (j in 1:length(testResults)) {
      check <- utils::capture.output(testResults[[j]])
      if (check[1] != "As expected ") failures %<>% `+`(1)
    }

    fileName <- singleTest$file[1]
    unitTest <- singleTest$test[1]

    if (failures > 0) {
      totalError %<>% `+`(1)
      if (logs) cat(paste0(Sys.time(), " : Failure in { ", fileName, " } --> ( ", unitTest, " ) \n"))
    } else {
      if (logs) cat(paste0(Sys.time(), " : Success in { ", fileName, " } --> ( ", unitTest, " ) \n"))
    }
  }

  # Calculate code status from running tests
  return(
    ifelse(
      test = totalError > 0,
      yes = 1,
      no = 0
    )
  )
}

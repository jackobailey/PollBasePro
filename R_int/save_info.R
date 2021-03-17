#' Quicky and Easy Session Info .txt Files
#'
#' This function provides a simple way to export session information.
#'
#' @param path The file path to where you would like to save your session information.
#' @examples
#' \dontrun{
#' save_info()
#' }
#' @export

save_info <- function(path = "session_info.txt"){

  sink(file = path)
  cat(paste0("Analysis completed: ", Sys.time(), "\n\n"))
  print(utils::sessionInfo())
  sink()

}


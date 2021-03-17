#' Load Jennings and Wlezien's "Timeline of Elections" Dataset
#'
#' This function downloads and loads the Timeline data.
#'
#' @examples
#' \dontrun{
#' load_timeline()
#' }
#' @export

load_timeline <- function(){

  # Download and extract CSES IMD data

  temp <- tempfile()
  utils::download.file("https://utexas.box.com/shared/static/6fevlf9v9s25ciky4vqdhj05rnk3d5ri.zip", temp)
  data <- utils::read.table(unz(temp, "Dataset-20180111.tab"), sep = "\t", header = T)
  unlink(temp)


  # Return data to user

  return(data)

}

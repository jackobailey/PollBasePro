#' Root-Mean-Square Error
#'
#' This function provides a simple way to compute RMSE.
#'
#' @param y1 A numeric vector.
#' @param y2 A numeric vector.
#'
#' @export

rmse <- function(y1, y2){
  sqrt(sum(((y1 - y2)^2)/length(y1)))
}


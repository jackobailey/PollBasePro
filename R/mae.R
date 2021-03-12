#' Mean Absolute Error
#'
#' This function provides a simple way to compute MAE.
#'
#' @param y1 A numeric vector.
#' @param y2 A numeric vector.
#'
#' @export

mae <- function(y1, y2){
  mean(abs(y1 - y2))
}


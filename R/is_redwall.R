#' Is a Constituency in the "Red Wall"?
#'
#' This function takes a vector of constituency names and returns a variable that indicates if each element is in the "Red Wall" or not. These constituencies are coded according to the classification created by James Kanagasooriam.
#'
#' @param x A vector of constituency data.
#' @return A boolean vector.
#' @examples
#' \dontrun{
#' is_redwall(x = "Bury South")
#' }
#' @export

is_red_wall <- function(x = NULL){

  # Check if x is in list of redwall seats

  rw <- x %in% c(britpol::red_wall$name, britpol::red_wall$gss_code)

  # Return the data to the user

  return(rw)

}


#' Is a Constituency in the "Red Wall"?
#'
#' This function takes a vector of constituency names and returns a variable that indicates if each element is in the "Red Wall" or not. These constituencies are coded according to the classification created by James Kanagasooriam that underpinned much of the Conservative's 2019 campaign.
#'
#' @param x A vector of constituency data.
#' @return A boolean vector.
#' @examples
#' \dontrun{
#' is_redwall(x = profile$pconW1)
#' }
#' @export

is_redwall <- function(x = NULL){

  # Convert the variable to a character variable

  if(haven::is.labelled(x) == T){
    x <- haven::as_factor(x)
  }

  x <- as.character(x)


  # Detect Red Wall seats

  x <-
    dplyr::case_when(
      x %in% c(
        "Bury South",
        "Bolton North East",
        "Oldham East and Saddleworth",
        "Heywood and Middleton",
        "Chorley",
        "Hyndburn",
        "Burnley",
        "Blackpool South",
        "Wirral South",
        "Scunthorpe",
        "Great Grimsby",
        "Penistone and Stocksbridge",
        "Rother Valley",
        "Don Valley",
        "Halifax",
        "Batley and Spen",
        "Wakefield",
        "Bradford South",
        "Hemsworth",
        "North West Durham",
        "Darlington",
        "Sedgefield",
        "Bishop Auckland",
        "Tynemouth",
        "Newcastle upon Tyne North",
        "Newcastle-under-Lyme",
        "Stoke-on-Trent Central",
        "Stoke-on-Trent North",
        "Coventry South",
        "Coventry North West",
        "Birmingham, Northfield",
        "Wolverhampton North East",
        "West Bromwich West",
        "Dudley North",
        "Chesterfield",
        "Bolsover",
        "Gedling",
        "Bassetlaw",
        "Ashfield"
      ) ~ TRUE,
      is.na(x) ~ NA,
      TRUE ~ FALSE
    )


  # Return the data to the user

  return(x)

}


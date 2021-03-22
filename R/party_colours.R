#' British Political Party Colours
#'
#' British parliamentary constituencies are not often coded in a consistent manner. Fixing this can also be very time-consuming. This function uses the constituencies dataset in the package to standardise a vector of constituencies names.
#'
#' @param x A vector of party names.
#' @return A vector of corresponding hex codes
#' @examples
#' party_colours(c("Ynys Môn", "Derby North", "North, Derby"))
#' @export

party_colours <- function(x){

  # Simplify party names

  x <-
    x %>%
    tolower() %>%
    stringr::str_remove_all("[[:punct:]]") %>%
    stringr::str_replace_all("[[:space:]]", "") %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    str_remove("the")


  # Detect colours

  colours <-
    dplyr::case_when(
      stringr::str_detect(x, "^con|^tor") ~ "#0087dc",
      stringr::str_detect(x, "^lab") ~ "#d50000",
      stringr::str_detect(x, "^lib|^ld") ~ "#FDBB30",
      stringr::str_detect(x, "^brex|^reform|^brx|^ref") ~ "#12B6CF",
      stringr::str_detect(x, "^ukip|^ukind|^unitedkingdom") ~ "#B3009D",
      stringr::str_detect(x, "^green|^grn") ~ "#008066",
      stringr::str_detect(x, "^snp|^scotnat|^scottishnat") ~ "#FFF95D",
      stringr::str_detect(x, "^pc|plaid") ~ "#3F8428",
      TRUE ~ "#a4a6a8"
    )


  # Return the simplified data to the user

  return(colours)

}

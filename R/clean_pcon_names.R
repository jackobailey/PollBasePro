#' Clean British Parliamentary Constituency Names
#'
#' British parliamentary constituencies are not often coded in a consistent manner. Fixing this can also be very time-consuming. This function uses the constituencies dataset in the package to standardise a vector of constituencies names.
#'
#' @param x A vector of constituency names.
#' @return A vector of cleaned constituency names.
#' @examples
#' clean_constituencies("Ynys Môn")
#' @export

clean_pcon_names <- function(x){


  x <- c("Aberavon", "Ynys Môn", "Derby North")

  # Convert names vector to lower case and remove punctuation, spaces, and diacritics

  x <-
    x %>%
    tolower() %>%
    stringr::str_remove_all("[[:punct:]]") %>%
    stringr::str_replace_all("  ", " ") %>%
    stringr::str_remove_all(" ") %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT")


  # Simplify reference names and split by " "

  ref <-
    constituencies$name %>%
    tolower() %>%
    stringr::str_remove_all("[[:punct:]]") %>%
    stringr::str_replace_all("  ", " ") %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    stringr::str_replace_all(" ", ")(?=.*") %>%
    stringr::str_replace_all("^", "(?=.*") %>%
    stringr::str_replace_all("$", ")")


  # Simplify names (let me know if you know how to speed this up)

  for(i in 1:length(ref)){
    x[stringr::str_detect(x, ref[i])] <- constituencies$name[which(ref == ref[i])[1]]
  }


  # Return the simplified data to the user

  return(x)

}

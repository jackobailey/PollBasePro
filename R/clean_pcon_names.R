#' Clean British Parliamentary Constituency Names
#'
#' British parliamentary constituencies are not often coded in a consistent manner. Fixing this can also be very time-consuming. This function uses the constituencies dataset in the package to standardise a vector of constituencies names.
#'
#' @param x A vector of constituency names.
#' @return A vector of cleaned constituency names.
#' @examples
#' clean_pcon_names(c("Ynys Môn", "Derby North", "North, Derby"))
#' @export

clean_pcon_names <- function(x){

  # Convert names vector to lower case and remove punctuation, spaces, and diacritics

  x <-
    x %>%
    tolower() %>%
    stringr::str_remove_all("[[:punct:]]") %>%
    stringr::str_replace_all("  ", " ") %>%
    stringr::str_replace_all(" ", "") %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    stringr::str_split("")



  # Simplify reference names and

  ref <-
    constituencies$name %>%
    tolower() %>%
    stringr::str_remove_all("[[:punct:]]") %>%
    stringr::str_replace_all("  ", " ") %>%
    stringr::str_replace_all(" ", "") %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    stringr::str_split("")


  # Simplify names (let me know if you know how to speed this up)

  for(i in 1:length(ref)){
    for(j in 1:length(x)){
      x[j][setequal(x[[j]], ref[[i]])] <- constituencies$name[i][[1]]
    }
  }

  # Unlist x

  x <- unlist(x)


  # Remove any unmatched names

  x[x == tolower(x)] <- NA


  # Return the simplified data to the user

  return(x)

}

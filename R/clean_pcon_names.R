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
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT")


  # Simplify reference names and

  ref <-
    constituencies$name %>%
    tolower() %>%
    stringr::str_remove_all("[[:punct:]]") %>%
    stringr::str_replace_all("  ", " ") %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    stringr::str_replace_all(" ", ")(?=.*") %>%
    stringr::str_replace_all("^", "^(?=.*") %>%
    stringr::str_replace_all("$", ").+$")


  # Reformat where the number of words == 1

  ref <-
    ifelse(
      stringr::str_count(ref, "\\*") == 1,
      paste0(
        "^",
        lapply(
          stringr::str_extract_all(
            ref,
            "[[:alpha:]]"),
          paste0,
          collapse = ""
        ),
        "$"
      ),
      ref
    )


  # Simplify names (let me know if you know how to speed this up)

  for(i in 1:length(ref)){
    x[stringr::str_detect(x, ref[i])] <- constituencies$name[which(ref == ref[i])[1]]
  }


  # Remove any unmatched names

  x[x == tolower(x)] <- NA


  # Return the simplified data to the user

  return(x)

}

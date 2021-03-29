#' Clean British Parliamentary Constituency Names
#'
#' British parliamentary constituencies are not often coded in a consistent manner. Fixing this can also be very time-consuming. This function uses the constituencies dataset in the package to standardise a vector of constituencies names. Note that where strings are uncertain this function can take a while to run.
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
    stringr::str_replace("&", "and") %>%
    stringr::str_remove_all("[[:punct:]]") %>%
    stringr::str_remove_all("\\bthe\\b") %>%
    stringr::str_replace("\\bsiar\\b", "an iar") %>%
    stringr::str_replace_all("[[:space:]]", "") %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    stringr::str_remove("kingstonupon") %>%
    stringr::str_remove("upontyne") %>%
    stringr::str_split("") %>%
    lapply(sort) %>%
    lapply(paste0, collapse = "") %>%
    unlist()



  # Simplify reference names and remove remove punctuation, etc.

  ref <-
    britpol::constituency_results$constituency %>%
    tolower() %>%
    stringr::str_replace("&", "and") %>%
    stringr::str_remove_all("[[:punct:]]") %>%
    stringr::str_remove_all("\\bthe\\b") %>%
    stringr::str_replace("\\bsiar\\b", "an iar") %>%
    stringr::str_replace_all("[[:space:]]", "") %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    stringr::str_remove("kingstonupon") %>%
    stringr::str_remove("upontyne") %>%
    stringr::str_split("") %>%
    lapply(sort) %>%
    lapply(paste0, collapse = "") %>%
    unlist()


  # Convert to names in constituencies dataset
  # (let me know if you know how to speed this up)

  for(i in 1:length(ref)){
    x[x == ref[i]] <- britpol::constituency_results$constituency[i]
  }


  # Make best guess of missing strings based on length of data

  if(length(x[x == tolower(x)]) > 0){
    for(i in which(x == tolower(x))){
      x[i] <- britpol::constituency_results$constituency[which.max(RecordLinkage::jarowinkler(ref, x[i]))]
    }
  }


  # Remove any unmatched names

  x[x == tolower(x)] <- NA


  # Print number of unmatched cases if > 0

  if(sum(is.na(x)) > 0){
    warning(
      paste0(
        "There ",
        ifelse(sum(is.na(x)) == 1, "was ", "were "),
        sum(is.na(x)), " unmatched ",
        ifelse(sum(is.na(x)) == 1, "case. ", "cases. "),
        ifelse(sum(is.na(x)) == 1, "It is ", "They are "),
        "now coded as NA."
      )
    )
  }


  # Return the simplified data to the user

  return(x)

}

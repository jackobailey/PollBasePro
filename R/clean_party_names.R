#' Clean British Political Party Names
#'
#' For a majoritarian democracy, Britain has quite a few parties. Further, some of the main parties come in more than one variant (e.g. Labour & Co-op). This function takes a vector of party names, simplifies them, and then returns them to the user. It also includes some useful options to customise the output.
#'
#' @param x A vector of party names.
#' @param party_names A list of regular expressions to detect for each party, followed by the name to give the party. Note, the function converts all text to lower-case and removes any whitespace prior to simplification. E.g. list("^con|^tor" = "Conservative", "^lab" = "Labour).
#' @param nat Include nationalist parties or mark them as "Other"? Defaults to TRUE and includes them by default.
#' @param drop_dk Mark "Don't know", "Refused", etc. as missing? Defaults to TRUE.
#' @return A vector of party names.
#' @examples
#' clean_party_names(c("Labour", "Lab", "Lib", "Conservatives", "Tories"))
#' @export

clean_party_names <- function(x, party_names = list("^con|^tor" = "Conservatives", "^lab" = "Labour", "^lib|^ld" = "Liberals etc.", "^snp|^scotnat|^scottishnat" = "SNP", "^pc|^plaid" = "Plaid Cymru"), nat = TRUE, drop_dk = TRUE){


  # Convert names vector to lower case and remove punctuation and spaces

  x <-
    x %>%
    tolower() %>%
    stringr::str_remove_all("[[:punct:]]") %>%
    stringr::str_replace_all("[[:space:]]", "") %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    stringr::str_remove("^the")


  # Get regular expressions

  expr <- names(party_names)


  # Remove SNP/PC if nat == F

  if(nat != TRUE){

    # Get indices that refer to SNP/PC

    nats <-
      expr %>%
      stringr::str_detect(
        "snp|scotnat|scottishnat|pc|plaid|nat"
      ) %>%
      which()


    # Mark them as other

    party_names[nats] <- "Other"


  }


  # Drop "Don't know" etc. if drop_dk == T

  if(drop_dk == T){

    x <-
      ifelse(
        stringr::str_detect(x, "^dk|^dontknow|^refused|^skip|^prefernot|^miss|^na"),
        NA,
        x
      )

  } else {

    x[stringr::str_detect(x, "^dk|^dontknow|^refused|^skip|^prefernot|^miss|^na")] <- "Other"

  }


  # Simplify names (let me know if you know how to speed this up)

  for(i in 1:length(party_names)){
    x[stringr::str_detect(x, expr[i])] <- party_names[[i]]
  }


  # Mark all other names as "Other"

  x[!x %in% unlist(party_names) & is.na(x) == F] <- "Other"


  # Return the simplified data to the user

  return(x)

}

#' Generate Variable Tables
#'
#' Provided a data frame of labelled vectors, this function returns a formatted summary table.
#'
#' @param data A data frame of labelled vectors
#' @export

make_table <- function(data = NULL){

  # Get pipe from Magrittr

  `%>%` <- magrittr::`%>%`


  # Get list of variable names

  var_names <- paste0("\\texttt{" , stringr::str_replace(names(data), "_", "\\\\_"), "}")


  # Get list of variable labels

  var_labels <- paste0(unlist(labelled::var_label(data)))


  # Convert to a data frame

  var_dta <-
    data.frame(
      `Name` = var_names,
      `Description` = var_labels
    )


  # Make data frame names use sans font

  names(var_dta) <- paste0("\\textsf{\\textbf{" , names(var_dta), "}}")


  # Convert to latex table

  var_tab <-
    kableExtra::kable(
      var_dta,
      align = "ll",
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      linesep = ""
    ) %>%
    kableExtra::kable_styling(
      position = "center",
      latex_options = "hold_position"
    )


  # Return table to user

  return(var_tab)

}


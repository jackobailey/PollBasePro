#' Subset by Date
#'
#' We sometimes need to subset high-frequency daily data to fit less high-frequency data that occurs on some other time interval. This function lets you pick the time range and does the subsetting for you.
#'
#' @param data A dataset that includes a date variable.
#' @param date The name of your date variable in your data.
#' @param freq What frequency to subset to. E.g. "week", "month", etc.
#' @return A tibble of data.
#' @examples
#' subset_date(data = pollbasepro, date = "date", freq = "week")
#' @export

subset_date <- function(data = NULL, date = "date", freq = "week"){

  # Return an error if the user provided no data

  if(is.null(data) == T){
    stop("You didn't provide any data! Use the 'data' argument (e.g. 'data = pollbase').")
  }


  # Return an error if the user didn't provide a date column

  if(is.null(date) == T){
    stop("You didn't provide a date column to use! Try the 'date' argument (e.g. date = 'date').")
  }


  # Return error if there is no date column based on the name they supplied

  if(length(names(data)[names(data) == date]) == 0){
    stop(
      paste0(
        "It doesn't look like your data contains a column called '",
        date,
        "'. Did you mean to use a different column? If so, try using the 'date' argument (e.g. date = 'end_point')."
      )
    )
  }


  # Check period name is ok

  if(!freq %in% c("week", "month", "bimonth", "quarter", "season", "halfyear", "year")){
    stop("Pick a valid frequency: week, month, bimonth, quarter, season, halfyear, or year.")
  }


  # Subset the data to the users frequency of choice

  data <-
    dplyr::filter(
      .data = data,
      date == (lubridate::ceiling_date(date, freq) - 1)
    )


  # Return data to user

  return(data)


}

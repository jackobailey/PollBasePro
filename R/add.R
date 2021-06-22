#' Add Prime Minister to Dated Datasets
#'
#' It's often useful to know who the Prime Minister was at any point in time when analysing political data. This function takes a dataset that includes a dated variable and adds to it a new column that shows who the Prime Minister was at each moment in time.
#'
#' @param data A dataset that includes a date variable.
#' @param date The name of your date variable in your data.
#' @param name What to call the new column of Prime Ministers. Defaults to "prime_minister".
#' @return A tibble of data.
#' @examples
#' dta <- data.frame(date = seq.Date(as.Date("2000-01-01"), as.Date("2020-01-01"), by = "year"))
#' add_pm(data = dta, date = "date", name = "prime_minister")
#' @export

add_pm <- function(data = NULL, date = "date", name = "prime_minister"){

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


  # Return an error if the user provide a non-character name

  if(is.character(name) == F){
    stop("The name needs to be a character vector! I.e. it should be in quotation marks!")
  }


  # Amend final PM to some absurd future data

  prime_ministers$end[nrow(prime_ministers)] <- "3000-01-01"


  # If everything's ok, return the relevant PM for each date

  data <-
    cbind(
      data,
      lookup_interval(
        lookup_date = data[[date]],
        interval_data = prime_ministers,
        variable = "prime_minister"
      )
    ) %>%
    dplyr::tibble()


  # Rename the column

  names(data)[names(data) == "prime_minister"] <- name


  # Return to user

  return(data)

}


#' Add Prime Minister's Party to Dated Datasets
#'
#' It's often useful to know which party the Prime Minister belonged to at any point in time when analysing political data. This function takes a dataset that includes a dated variable and adds to it a new column that shows the party of the Prime Minister at each moment in time.
#'
#' @param data A dataset that includes a date variable.
#' @param date The name of your date variable in your data.
#' @param name What to call the new column of Prime Ministers. Defaults to "pm_party".
#' @return A tibble of data.
#' @examples
#' dta <- data.frame(date = seq.Date(as.Date("2000-01-01"), as.Date("2020-01-01"), by = "year"))
#' add_pm_party(data = dta, date = "date", name = "pm_party")
#' @export

add_pm_party <- function(data = NULL, date = "date", name = "pm_party"){

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


  # Return an error if the user provide a non-character name

  if(is.character(name) == F){
    stop("The name needs to be a character vector! I.e. it should be in quotation marks!")
  }


  # If everything's ok, return the data for each date

  data <-
    cbind(
      data,
      lookup_interval(
        lookup_date = data[[date]],
        interval_data = prime_ministers,
        variable = "pm_party"
      )
    ) %>%
    dplyr::tibble()


  # Rename the column

  names(data)[names(data) == "pm_party"] <- name


  # Return to user

  return(data)

}



#' Add Election Dates to Dated Datasets
#'
#' It's often useful to know when elections occurred relative to the cases in your data. This function takes a dataset that includes a dated variable and adds to it the date of the last election, next election, or both.
#'
#' @param data A dataset that includes a date variable.
#' @param date The name of your date variable in your data.
#' @param last_name What to call the new column of last election dates. Defaults to "last_elec".
#' @param next_name What to call the new column of next election dates. Defaults to "next_elec".
#' @param which Which elections to return: "last", "next", or "both". Defaults to "both".
#' @return A tibble of data.
#' @examples
#' add_elections(data = pollbasepro, date = "date", which = "both")
#' @export

add_elections <- function(data = NULL, date = "date", last_name = "last_elec", next_name = "next_elec", which = "both"){

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


  # Return an error if the user provide a non-character name

  if(is.character(last_name) == F){
    stop("The last_name needs to be a character vector! I.e. it should be in quotation marks!")
  } else if(is.character(next_name) == F){
    stop("The next_name needs to be a character vector! I.e. it should be in quotation marks!")
  }


  # Create dataset of election pairs

  election_dates <-
    britpol::election_dates %>%
    dplyr::rename(start = date) %>%
    dplyr::mutate(
      end =
        ifelse(
          is.na(dplyr::lead(start, 1)) == T,
          Sys.Date(),
          dplyr::lead(start, 1)
        ) %>%
        lubridate::as_date(),
      start_id = start,
      end_id = end
    )


  # Return the data for each date

  if(which == "last"){

    # Join data

    data <-
      cbind(
        data,
        lookup_interval(
          lookup_date = data[[date]],
          interval_data = election_dates,
          variable = "start_id"
        )
      ) %>%
      dplyr::tibble()

    # Rename the column

    names(data)[names(data) == "start_id"] <- last_name


  } else if (which == "next"){

    # Join data

    data <-
      cbind(
        data,
        lookup_interval(
          lookup_date = data[[date]],
          interval_data = election_dates,
          variable = "end_id"
        )
      ) %>%
      dplyr::tibble()

    # Rename the column

    names(data)[names(data) == "end_id"] <- next_name

  } else if (which == "both"){

    # Join data

    data <-
      cbind(
        data,
        lookup_interval(
          lookup_date = data[[date]],
          interval_data = election_dates,
          variable = "start_id"
        ),
        lookup_interval(
          lookup_date = data[[date]],
          interval_data = election_dates,
          variable = "end_id"
        )
      ) %>%
      dplyr::tibble()

    # Rename the column

    names(data)[names(data) == "start_id"] <- last_name
    names(data)[names(data) == "end_id"] <- next_name

  }


  # Return to user

  return(data)

}


#' Lookup Column Values Based on Date Intervals
#'
#' Sometimes we need to lookup a value at a particular time based on a date range in another dataset. This function provides a wrapper to data.table to make this quick and easy.
#'
#' @param lookup_date A column of dated data.
#' @param interval_data A dataset including columns called "start" and "end".
#' @param variable The variable to select values from in the interval_data.
#' @return A tibble of data.

lookup_interval <- function(lookup_date, interval_data, variable){


  # Convert data to data.tables

  lookup_date <- data.table::data.table(date = lookup_date)
  interval_data <- data.table::as.data.table(interval_data)


  # Join data

  # dta <-
  #   data.table:::`[.data.table`(
  #     x = interval_data,
  #     i = lookup_date,
  #     on = list(end > date, start <= date),
  #     j = ..variable
  #   )

  dta <- interval_data[lookup_date, ..variable, on = list(end > date, start <= date)]

  # Convert to tibble

  dta <- dplyr::tibble(dta)


  # Return to user

  return(dta)

}


#' Add Lead of One Party Over Another
#'
#' We often want to know how much further one party is ahead of another in the polls. But this can be tricky when the polls contain error. This function takes a dataset with columns named "{{party}}_est" and "{{party}}_err", computes the lead of one party over another, and appends it plus the error in the estimate to the data.
#'
#' @param data A dataset containing variables called {{party}}_est and {{party}}_err
#' @param party1 The name of party
#' @param party2 The name of your date variable in your data.
#' @param name The name . Defaults to "lead".
#' @return A tibble of data.
#' @examples
#' add_lead(data = pollbasepro, party1 = "con", party2 = "lab")
#' @export

add_lead <- function(data = NULL, party1 = "con", party2 = "lab", name = "lead"){

  # Return an error if the user provided no data

  if(is.null(data) == T){
    stop("You didn't provide any data! Use the 'data' argument (e.g. 'data = pollbase').")
  }


  # Return an error if the user didn't provide the parties

  if(is.null(party1) == T){
    stop("You didn't provide a first party to use! Try the 'party1' argument (e.g. party1 = 'con').")
  } else if(is.null(party2) == T){
    stop("You didn't provide a second party to use! Try the 'party1' argument (e.g. party2 = 'lab').")
  }


  # Compute lead

  lead <-
    dplyr::tibble(
      lead_est = data[[paste0(party1, "_est")]] - data[[paste0(party2, "_est")]],
      lead_err = sqrt(data[[paste0(party1, "_err")]]^2 + data[[paste0(party2, "_err")]]^2)
    )


  # Rename variables

  names(lead) <- paste0(name, c("_est", "_err"))


  # Append to data

  data <- dplyr::tibble(cbind(data, lead))


  # Return data to user

  return(data)


}


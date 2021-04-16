#' Create New Polling Universe
#'
#' Polls contain all sorts of uncertainty. But people still read them as if every little shift has some important cause, rather than simply resulting from random chance. This function uses `pollbasepro` to generate a new synthetic dataset of plausible polling results. These new "polls" line up with the days and sample sizes in the `pollbase` data. Note: the function doesn't currently account for house effects.
#'
#' @param start The date on which to start your new polling universe, in ymd (YYYY-MM-DD) format.
#' @param end The date on which to end your new polling universe, in ymd (YYYY-MM-DD) format.
#' @return A tibble of polling data.
#' @examples
#' pollingverse <- polling_multiverse(start = "2017-06-18", end = "2019-12-12")
#' @export

polling_multiverse <- function(start = NULL, end = NULL){

  # Error if dates not specified
  if(is.null(start) == T || is.null(end) == T ||
     is.na(as.Date(start)) || is.na(as.Date(end))){
    stop("Please provide a valid date range of the form YYYY-MM-DD using the start and end arguments.")
  }


  # Warning if dates seem implausible or suggest YMD mixed up
  if(as.Date(start) < as.Date("0032-01-01") ||
     as.Date(end) < as.Date("0032-01-01")){
    warning("Make sure you specified your start and end dates in YYYY-MM-DD format.")
  }


  # Error if end date before start date
  if(as.Date(end) < as.Date(start)){
    stop("The end date of your search must be later than the start date.")
  }


  # Error if date range rules out finding any polls
  if(as.Date(end) < min(britpol::pollbasepro$date) ||
     as.Date(start) > max(britpol::pollbasepro$date)){
    stop("The date range you have specified is outside of the range covered by available polling data.")
  }


  # Convert start and end to dates

  if(is.null(start) == F){
    start <- as.Date(start)
  } else if(is.null(end) == F){
    end <- as.Date(end)
  }


  # Get pollbase data

  pollbase <-
    britpol::pollbase %>%
    dplyr::filter(end >= min(britpol::pollbasepro$date))


  # Subset if necessary

  if(is.null(start) == F){

    pollbase <-
      pollbase %>%
      dplyr::filter(end >= {{start}})

  }

  if(is.null(end) == F){

    pollbase <-
      pollbase %>%
      dplyr::filter(end <= {{end}})

  }


  # Add imputed sample sizes

  pollbase <-
    dplyr::left_join(
      pollbase,
      britpol::samplesizes,
      by = c("end" = "date")
    ) %>%
    dplyr::mutate(
      n = ifelse(is.na(n) == T, round(n_est, 0), n)
    ) %>%
    dplyr::select(
      "id",
      "date" = "end",
      "n"
    )


  # Get appropriate days from pollbasepro

  pollbasepro <-
    dplyr::left_join(
      pollbase,
      britpol::pollbasepro,
      "date"
    ) %>%
    stats::na.omit()


  # Sample each row once from a binomial distribution

  pollingverse <-
    pollbasepro %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      con = stats::rbinom(1, size = n, prob = stats::rnorm(1e3, con_est, con_err))/n,
      lab = stats::rbinom(1, size = n, prob = stats::rnorm(1e3, lab_est, lab_err))/n,
      lib = stats::rbinom(1, size = n, prob = stats::rnorm(1e3, lib_est, lib_err))/n
    ) %>%
    dplyr::select(
      id,
      date,
      con,
      lab,
      lib
    ) %>%
    dplyr::ungroup()


  # Warn if date range includes no polls

  if(nrow(pollingverse) == 0){
    warning("No data returned, it is possible there is no polling data available for the date range you specified.")
  }



  # Return new polling universe to user

  return(pollingverse)

}

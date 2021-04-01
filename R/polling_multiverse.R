#' Create New Polling Universe
#'
#' Polls contain all sorts of uncertainty. But people still read them as if every little shift has some important cause, rather than simply resulting from random chance. This function uses `pollbasepro` to generate a new synthetic dataset of plausible polling results. These new "polls" line up with the days and sample sizes in the `pollbase` data. Note: the function doesn't currently account for house effects.
#'
#' @param start The date on which to start your new polling universe.
#' @param end The date on which to end your new polling universe.
#' @return A tibble of polling data.
#' @examples
#' polling_multiverse(start = "2017-06-18", end = "2019-12-12")
#' @export

polling_multiverse <- function(start = NULL, end = NULL){

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

  } else if(is.null(end) == F){

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
      britpol::pollbasepro,
      pollbase,
      "date"
    ) %>%
    na.omit()


  # Sample each row once from a binomial distribution

  pollbasepro <-
    pollbasepro %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      con = rbinom(1, size = n, prob = rnorm(1e3, con_est, con_err))/n,
      lab = rbinom(1, size = n, prob = rnorm(1e3, lab_est, lab_err))/n,
      lib = rbinom(1, size = n, prob = rnorm(1e3, lib_est, lib_err))/n
    ) %>%
    dplyr::select(
      date,
      con,
      lab,
      lib
    )


  # Arrange data by date

  pollbase <- dplyr::arrange(pollbase, date)
  pollbasepro <-
    pollbasepro %>%
    dplyr::arrange(date) %>%
    dplyr::select(-date)


  # Join data objects

 pollbase <- cbind(pollbase, pollbasepro)



  # Return new polling universe to user

  return(pollbase)

}

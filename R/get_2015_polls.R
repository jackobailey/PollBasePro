#' Get Voting Intention Polls, 2010-2015
#'
#' This function downloads and tidies polling data for the 2010 to 2015 period from Wikipedia.
#'
#' @export

get_2015_polls <- function(){

  # Get pipe from Magrittr

  `%>%` <- magrittr::`%>%`


  # We'll get the data we need from Wikipedia. This is useful because there
  # are a whole host of people who keep these pages up to date and it means
  # that we don't have to manage data collection ourselves.

  url1 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2015_United_Kingdom_general_election_(2010–2012)"
  url2 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2015_United_Kingdom_general_election"


  # Now, we'll use the htmltab() function to scrape the contents of the tables
  # on the page.

  dta_10 <-
    htmltab::htmltab(url1, 3) %>%
    dplyr::select(
      date = `Date(s)conducted`,
      pollster = `Polling organisation/client`,
      n = `Sample size`,
      con = Con,
      lab = Lab,
      lib = `Lib Dem`
    ) %>%
    dplyr::mutate(date = paste(date, "2010"))

  dta_11 <-
    htmltab::htmltab(url1, 2) %>%
    dplyr::select(
      date = `Date(s)conducted`,
      pollster = `Polling organisation/client`,
      n = `Sample size`,
      con = Con,
      lab = Lab,
      lib = `Lib Dem`
    ) %>%
    dplyr::mutate(date = paste(date, "2011"))

  dta_12 <-
    htmltab::htmltab(url1, 1) %>%
    dplyr::select(
      date = `Date(s)conducted`,
      pollster = `Polling organisation/client`,
      n = `Sample size`,
      con = Con,
      lab = Lab,
      lib = `Lib Dem`
    ) %>%
    dplyr::mutate(date = paste(date, "2012"))

  dta_13 <-
    htmltab::htmltab(url2, 5) %>%
    dplyr::select(
      date = `Date(s)conducted`,
      pollster = `Polling organisation/client`,
      n = `Sample size`,
      con = Con,
      lab = Lab,
      lib = `Lib Dem`
    ) %>%
    dplyr::mutate(date = paste(date, "2013"))

  dta_14 <-
    htmltab::htmltab(url2, 4) %>%
    dplyr::select(
      date = `Date(s)conducted`,
      pollster = `Polling organisation/client`,
      n = `Sample size`,
      con = Con,
      lab = Lab,
      lib = `Lib Dem`
    ) %>%
    dplyr::mutate(date = paste(date, "2014"))

  dta_15 <-
    htmltab::htmltab(url2, 3) %>%
    dplyr::select(
      date = `Date(s)conducted`,
      pollster = `Polling organisation/client`,
      n = `Sample size`,
      con = Con,
      lab = Lab,
      lib = `Lib Dem`
    ) %>%
    dplyr::mutate(date = paste(date, "2015"))


  # Now we'll merge the data together

  dta <- rbind(dta_10, dta_11, dta_12, dta_13, dta_14, dta_15)


  # At the moment, the data contain rows that include only information on the
  # events that came to pass in UK politics. We don't need these, so we will
  # remove them from the data.

  dta <-
    dta %>%
    dplyr::filter(!(pollster == con))


  # We'll also remove the actual election results themselves.

  dta <-
    dta %>%
    dplyr::filter(stringr::str_detect(pollster, "election|Election") == F)


  # Likewise we'll remove the clients from the pollster column and
  # convert the pollsters to lower case

  dta <-
    dta %>%
    dplyr::mutate(
      pollster =
        pollster %>%
        stringr::str_remove("/.*") %>%
        stringr::str_remove("-|\u2014") %>%
        tolower()
    )


  # Next, we'll convert the sample sizes and voting intention figures to
  # numeric vectors

  dta <-
    dta %>%
    dplyr::mutate(
      n =
        n %>%
        stringr::str_remove(",") %>%
        as.numeric(),
      con =
        con %>%
        stringr::str_remove("%") %>%
        as.numeric() %>%
        magrittr::divide_by(100),
      lab =
        lab %>%
        stringr::str_remove("%") %>%
        as.numeric() %>%
        magrittr::divide_by(100),
      lib =
        lib %>%
        stringr::str_remove("%") %>%
        as.numeric() %>%
        magrittr::divide_by(100)
    )


  # Now we can split the fieldwork dates into a start and end date.

  dta <-
    dta %>%
    tidyr::separate(
      col = date,
      sep = "-|–",
      into = c("start", "end")
    )


  # We'll now convert the end and start columns into date format

  dta <-
    dta %>%
    dplyr::mutate(
      end =
        ifelse(is.na(end) == T, start, end) %>%
        lubridate::dmy(),
      start =
        dplyr::case_when(
          nchar(start) <= 2 ~ paste(start, lubridate::month(end, label = T, abbr = T), lubridate::year(end)),
          nchar(start) > 2 & nchar(start) <= 7 ~ paste(start, lubridate::year(end)),
          nchar(start) > 7 ~ start
        ) %>%
        lubridate::dmy()
    ) %>%
    dplyr::mutate(
      start =
        lubridate::as_date(ifelse(is.na(start) == T, end, start)),
      start =
        lubridate::as_date(ifelse(start > end, start - lubridate::years(1), start))
    )


  # Now we'll return the data to the user

  return(dta)


}


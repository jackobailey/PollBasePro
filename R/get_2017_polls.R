#' Get Voting Intention Polls, 2015-2017
#'
#' This function downloads and tidies polling data for the 2015 to 2017 period from Wikipedia.
#'
#' @export

get_2017_polls <- function(){

  # Get pipe from Magrittr

  `%>%` <- magrittr::`%>%`


  # We'll get the data we need from Wikipedia. This is useful because there
  # are a whole host of people who keep these pages up to date and it means
  # that we don't have to manage data collection ourselves.

  url <- "https://w.wiki/365p"


  # Now, we'll use the htmltab() function to scrape the contents of the tables
  # on the page.

  dta_15 <-
    htmltab::htmltab(url, 4) %>%
    dplyr::select(
      date = `Date(s)conducted`,
      pollster = `Polling organisation/client`,
      n = `Sample size`,
      con = Con,
      lab = Lab,
      lib = `Lib Dem`
    ) %>%
    dplyr::mutate(date = paste(date, "2015"))

  dta_16 <-
    htmltab::htmltab(url, 3) %>%
    dplyr::select(
      date = `Date(s)conducted`,
      pollster = `Polling organisation/client`,
      n = `Sample size`,
      con = Con,
      lab = Lab,
      lib = `Lib Dem`
    ) %>%
    dplyr::mutate(date = paste(date, "2016"))

  dta_17 <-
    htmltab::htmltab(url, 2) %>%
    dplyr::select(
      date = `Date(s)conducted`,
      pollster = `Polling organisation/client`,
      n = `Sample size`,
      con = Con,
      lab = Lab,
      lib = `Lib Dem`
    ) %>%
    dplyr::mutate(date = paste(date, "2017"))


  # Now we'll merge the data together

  dta <- rbind(dta_15, dta_16, dta_17)


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


  # The date column includes non-ASCII chaaracters. Let's sub them out.

  dta <-
    dta %>%
    dplyr::mutate(
      date = iconv(date, "latin1", "ASCII", sub="-"),
      pollster = iconv(pollster, "latin1", "ASCII", sub="-")
    )


  # Likewise we'll remove the clients from the pollster column and
  # convert the pollsters to lower case

  dta <-
    dta %>%
    dplyr::mutate(
      pollster =
        pollster %>%
        stringr::str_remove("/.*") %>%
        stringr::str_remove("---") %>%
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
      sep = "---",
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


#' Jack Bailey's Colour Palette.
#'
#' This is a convenience function that makes it easier for me to make plots that use colours I like.
#' @param ... The name of the colour you want.
#'

bailey_colours <- function(...) {

  choice <- c(...)

  cols <- c(
    `red` = "#D10B0C",
    `red2` = "#D52122",
    `red3` = "#D93738",
    `red4` = "#DD4D4E",
    `red5` = "#E16364",
    `red6` = "#E5797A",
    `red7` = "#EA9090",
    `red8` = "#EEA6A6",
    `red9` = "#F2BCBC",
    `red10` = "#F6D2D2",
    `blue` = "#0D68C3",
    `blue2` = "#2375C8",
    `blue3` = "#3983CD",
    `blue4` = "#4F91D3",
    `blue5` = "#659ED8",
    `blue6` = "#7BACDE",
    `blue7` = "#91BAE3",
    `blue8` = "#A7C8E9",
    `blue9` = "#BDD5EE",
    `blue10` = "#D3E3F4",
    `grey` = "#5C5C5C",
    `grey2` = "#6A6A6A",
    `grey3` = "#797979",
    `grey4` = "#888888",
    `grey5` = "#979797",
    `grey6` = "#A6A6A6",
    `grey7` = "#B4B4B4",
    `grey8` = "#C3C3C3",
    `grey9` = "#D2D2D2",
    `grey10` = "#E1E1E1",
    `black` = "#000000",
    `white` = "#FFFFFF"
  )

  if (is.null(choice)) return(cols)

  `names<-`(cols[choice], NULL)
}


#' Fit Poll Pooling Model
#'
#' This function provides a simple way to fit a variant of Simon Jackman's "polling the polls" model.
#'
#' @param data Raw pollbase data.
#' @param init Initial election date.
#' @param final Final election date.
#' @param party A string reflecting a party. Either "con", "lab", or "lib".
#' @param alpha_init The party's vote share at the initial election.
#' @param alpha_final The party's vote share at the final election.
#' @param refresh How often to report model iterations. Defaults to 0.

fit_model <- function(data, init, final, party, alpha_init, alpha_final, refresh = 0){


  # Print party and election

  print(paste0("Fitting model: ", party, " ", init))


  # Add elections to data

  data <- britpol::add_elections(data, which = "last")


  # Subset data

  data <- data[data$last_elec == init, ]


  # Run model

  if(init < "2019-12-12"){

    # Compile Stan model

    model <-
      cmdstanr::cmdstan_model(
        here::here("models", "model_2anchors.stan"),
        cpp_options = list(stan_threads = TRUE)
      )


    # Create data for Stan

    stan_data <-
      list(
        N = nrow(data),
        T = (lubridate::interval(init, final)/lubridate::days(1)) + 1,
        P = length(unique(data$pollster)),
        y = data[[party]],
        s = sqrt((data[[party]]*(1 - data[[party]]))/(ifelse(is.na(data$n) == T, data$n_est, data$n)/data$days)),
        index = data$index,
        pollster = as.numeric(factor(data$pollster)),
        alpha_init = alpha_init,
        alpha_final = alpha_final
      )


    # Sample from model

    fit <-
      model$sample(
        data = stan_data,
        seed = 666,
        chains = 4,
        parallel_chains = 4,
        threads_per_chain = 3,
        refresh = refresh,
        show_messages = FALSE,
        init =
          list(
            list(alpha = rep(alpha_init, nrow(data))),
            list(alpha = rep(alpha_init, nrow(data))),
            list(alpha = rep(alpha_init, nrow(data))),
            list(alpha = rep(alpha_init, nrow(data)))
          ),
        adapt_delta = 0.99,
        max_treedepth = 15
      )


    # Get model summary

    mod_sum <-
      fit$summary(variables = "alpha") %>%
      dplyr::filter(stringr::str_detect(variable, "alpha") == T) %>%
      dplyr::mutate(
        index =
          variable %>%
          stringr::str_remove("alpha\\[") %>%
          stringr::str_remove("\\]"),
        date = lubridate::as_date({{init}}) + (dplyr::row_number() - 1)
      ) %>%
      dplyr::select(
        date,
        est = mean,
        sd
      ) %>%
      dplyr::mutate(
        party = {{party}}
      )


    # Remove final case to prevent duplication

    mod_sum <- mod_sum[1:(nrow(mod_sum)-1), ]


  } else if(init == "2019-12-12"){

    # Compile Stan model

    model <-
      cmdstanr::cmdstan_model(
        here::here("models", "model_1anchor.stan"),
        cpp_options = list(stan_threads = TRUE)
      )


    # Create data for Stan

    stan_data <-
      list(
        N = nrow(data),
        T = round((lubridate::interval(init, Sys.Date())/lubridate::days(1)) + 1, 0),
        P = length(unique(data$pollster)),
        y = data[[party]],
        s = sqrt((data[[party]]*(1 - data[[party]]))/(ifelse(is.na(data$n) == T, data$n_est, data$n)/data$days)),
        index = data$index,
        pollster = as.numeric(factor(data$pollster)),
        alpha_init = alpha_init
      )


    # Sample from model

    fit <-
      model$sample(
        data = stan_data,
        seed = 666,
        chains = 4,
        parallel_chains = 4,
        threads_per_chain = 3,
        refresh = 0,
        show_messages = FALSE,
        init =
          list(
            list(alpha = rep(alpha_init, nrow(data))),
            list(alpha = rep(alpha_init, nrow(data))),
            list(alpha = rep(alpha_init, nrow(data))),
            list(alpha = rep(alpha_init, nrow(data)))
          ),
        adapt_delta = 0.99,
        max_treedepth = 15
      )

    # Get model summary

    mod_sum <-
      fit$summary(variables = "alpha") %>%
      dplyr::filter(stringr::str_detect(variable, "alpha") == T) %>%
      dplyr::mutate(
        index =
          variable %>%
          stringr::str_remove("alpha\\[") %>%
          stringr::str_remove("\\]"),
        date = lubridate::as_date({{init}}) + (dplyr::row_number() - 1)
      ) %>%
      dplyr::select(
        date,
        est = mean,
        sd
      ) %>%
      dplyr::mutate(
        party = {{party}}
      )

  }


  # Return model summary to user

  return(mod_sum)

}


#' Get Voting Intention Polls, 2010-2015
#'
#' This function downloads and tidies polling data for the 2010 to 2015 period from Wikipedia.
#'

get_2015_polls <- function(){


  # We'll get the data we need from Wikipedia. This is useful because there
  # are a whole host of people who keep these pages up to date and it means
  # that we don't have to manage data collection ourselves.

  url1 <- "https://w.wiki/365k"
  url2 <- "https://w.wiki/365o"


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

#' Get Voting Intention Polls, 2015-2017
#'
#' This function downloads and tidies polling data for the 2015 to 2017 period from Wikipedia.
#'

get_2017_polls <- function(){


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

#' Get Voting Intention Polls, 2017-2019
#'
#' This function downloads and tidies polling data for the 2017 to 2019 period from Wikipedia.
#'

get_2019_polls <- function(){


  # We'll get the data we need from Wikipedia. This is useful because there
  # are a whole host of people who keep these pages up to date and it means
  # that we don't have to manage data collection ourselves.

  url <- "https://w.wiki/35RK"


  # Now, we'll use the htmltab() function to scrape the contents of the tables
  # on the page.

  dta_17 <-
    htmltab::htmltab(url, 4) %>%
    dplyr::select(
      date = `Date(s)conducted`,
      pollster = `Pollster/client(s)`,
      n = `Samplesize`,
      con = Con,
      lab = Lab,
      lib = `Lib Dem`
    ) %>%
    dplyr::mutate(date = paste(date, "2017"))

  dta_18 <-
    htmltab::htmltab(url, 3) %>%
    dplyr::select(
      date = `Date(s)conducted`,
      pollster = `Pollster/client(s)`,
      n = `Samplesize`,
      con = Con,
      lab = Lab,
      lib = `Lib Dem`
    ) %>%
    dplyr::mutate(date = paste(date, "2018"))

  dta_19 <-
    htmltab::htmltab(url, 2) %>%
    dplyr::select(
      date = `Date(s)conducted`,
      pollster = `Pollster/client(s)`,
      n = `Samplesize`,
      con = Con,
      lab = Lab,
      lib = `Lib Dem`
    ) %>%
    dplyr::mutate(date = paste(date, "2019"))


  # Now we'll merge the data together

  dta <- rbind(dta_17, dta_18, dta_19)


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

#' Get Voting Intention Polls, 2019-Present
#'
#' This function downloads and tidies polling data for the 2019-Present period from Wikipedia.
#'

get_new_polls <- function(){


  # We'll get the data we need from Wikipedia. This is useful because there
  # are a whole host of people who keep these pages up to date and it means
  # that we don't have to manage data collection ourselves.

  url <- "https://w.wiki/365q"


  # Now, we'll use the htmltab() function to scrape the contents of the tables
  # on the page.

  dta_20 <-
    htmltab::htmltab(url, 3) %>%
    dplyr::select(
      date = Datesconducted,
      pollster = Pollster,
      n = Samplesize,
      con = Con,
      lab = Lab,
      lib = `Lib Dem`
    ) %>%
    dplyr::mutate(date = paste(date, "2020"))

  dta_21 <-
    htmltab::htmltab(url, 2) %>%
    dplyr::select(
      date = Datesconducted,
      pollster = Pollster,
      n = Samplesize,
      con = Con,
      lab = Lab,
      lib = `Lib Dem`
    ) %>%
    dplyr::mutate(date = paste(date, "2021"))


  # Now we'll merge the data together

  dta <- rbind(dta_20, dta_21)


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
        stringr::str_remove("[^0-9]") %>%
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
    dplyr::mutate(
      date =
        dplyr::case_when(
          stringr::str_detect(date, "---") == F ~ paste0(date, "---", date),
          TRUE ~ date
        )
    ) %>%
    tidyr::separate(
      col = date,
      sep = "---| --- ",
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

#' In-Text Summary Statistics
#'
#' Generate in-text summary statistics for posterior distributions from Bayesian models. Most useful when used in Rmarkdown documents.
#'
#' @param x The distribution to summarise
#' @param text A character string that follows and describes the point estimate.
#' @param suffix A character string that follows each estimate.
#' @param inside Should the median be inside or outside the brackets? Defaults to T.
#' @param probs The level at which to compute the lower and upper bounds. Defaults to 0.95.
#' @param digits How many digits to round the summary statistics to. Defaults to 1.
#' @return A text string.

in_text <- function(x, text = NULL, suffix = NULL, inside = T, probs = .95, digits = 1){

  # Get median of the distribution and round to
  # desired number of digits

  m <- stats::median(x)
  m <- round(m, digits = digits)
  m <- format(m, nsmall = digits)


  # Get lower bound and round to desired digits

  l <- stats::quantile(x, probs = (1 - probs)/2)
  l <- round(l, digits = digits)
  l <- format(l, nsmall = digits)


  # Get upper bound and round to desired digits

  u <- stats::quantile(x, probs = probs + ((1 - probs)/2))
  u <- round(u, digits = digits)
  u <- format(u, nsmall = digits)


  # Add suffix if necessary

  if(is.null(suffix) == F){
    m <- paste0(m, suffix)
    l <- paste0(l, suffix)
    u <- paste0(u, suffix)
  }


  # Add in text if necessary

  if(is.null(text) == F){
    m <- paste0(m, text)
  }


  # Return in-text version

  if(inside == T){

    paste0("(", m, ", ", probs*100, "% ", "CI: ", l, " to ", u, ")")

  } else {

    paste0(m, " (", probs*100, "% ", "CI: ", l, " to ", u, ")")

  }

}

#' Load Jennings and Wlezien's "Timeline of Elections" Dataset
#'
#' This function downloads and loads the Timeline data.

load_timeline <- function(){

  # Download and extract CSES IMD data

  temp <- tempfile()
  utils::download.file("https://utexas.box.com/shared/static/6fevlf9v9s25ciky4vqdhj05rnk3d5ri.zip", temp)
  data <- utils::read.table(unz(temp, "Dataset-20180111.tab"), sep = "\t", header = T)
  unlink(temp)


  # Return data to user

  return(data)

}


#' Mean Absolute Error
#'
#' This function provides a simple way to compute MAE.
#'
#' @param y1 A numeric vector.
#' @param y2 A numeric vector.
#'

mae <- function(y1, y2){
  mean(abs(y1 - y2))
}

#' Generate Variable Tables
#'
#' Provided a data frame of labelled vectors, this function returns a formatted summary table.
#'
#' @param data A data frame of labelled vectors

make_table <- function(data = NULL){


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

#' Root-Mean-Square Error
#'
#' This function provides a simple way to compute RMSE.
#'
#' @param y1 A numeric vector.
#' @param y2 A numeric vector.
#'

rmse <- function(y1, y2){
  sqrt(sum(((y1 - y2)^2)/length(y1)))
}

#' Quicky and Easy Session Info .txt Files
#'
#' This function provides a simple way to export session information.
#'
#' @param path The file path to where you would like to save your session information.

save_info <- function(path = "session_info.txt"){

  sink(file = path)
  cat(paste0("Analysis completed: ", Sys.time(), "\n\n"))
  print(utils::sessionInfo())
  sink()

}

#' Update Voting Intention Polls, 2019-Present
#'
#' This function updates the pollbasepro data to include the latest polling data and outputs Twitter content.
#'
#'

update_pollbasepro <- function(){

  # Get election results

  election_results <-
    britpol::constituency_results %>%
    dplyr::filter(
      !region %in% c("University", "Northern Ireland"),
      sum(c(con, lab, lib, nat, oth), na.rm = T) >0
    ) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      total = sum(c(con, lab, lib, nat, oth), na.rm = T),
      con = sum(con, na.rm = T)/total,
      lab = sum(lab, na.rm = T)/total,
      lib = sum(lib, na.rm = T)/total
    ) %>%
    tidyr::pivot_longer(
      cols = c(con, lab, lib),
      names_to = "party",
      values_to = "results"
    ) %>%
    dplyr::filter(date == max(date))



  # Make list object

  dta <-
    tibble::tibble(
      init = "2019-12-12",
      final = NA,
      con = NA,
      lab = NA,
      lib = NA
    ) %>%
    tidyr::pivot_longer(
      cols = c(con, lab, lib),
      names_to = "party",
      values_to = "estimates"
    ) %>%
    dplyr::mutate(
      alpha_init = election_results$results[election_results$party == party],
      alpha_final = NA,
      estimates = as.list(rep(NA, dplyr::n()))
    )


  # Get wiki data and mutate

  wiki <-
    get_new_polls() %>%
    dplyr::mutate(
      election = "2019-12-12",
      id =
        paste0(
          "poll-",
          seq(
            as.numeric(
              stringr::str_remove(
                britpol::pollbase$id[britpol::pollbase$end > "2019-12-12"][1], ".*-"
              )
            ),
            as.numeric(
              stringr::str_remove(
                britpol::pollbase$id[britpol::pollbase$end > "2019-12-12"][1], ".*-"
              )
            ) + nrow(.) - 1,
            by = 1
          )
        ),
      days =
        lubridate::interval(start, end) %>%
        magrittr::divide_by(lubridate::days(1)) %>%
        magrittr::add(1)
    ) %>%
    tidyr::uncount(days) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
      date = lubridate::`%m+%`(start, lubridate::days(dplyr::row_number() - 1)),
      days = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-start, -end) %>%
    dplyr::mutate(
      index = (lubridate::interval(election, date)/lubridate::days(1)) + 1
    ) %>%
    dplyr::select(
      id,
      days,
      date,
      election,
      pollster,
      n,
      con,
      lab,
      lib,
      index
    )


  # Use hotdeck imputation to fill in missing sample sizes

  wiki$n[is.na(wiki$n) == T] <- round(stats::median(wiki$n, na.rm = T), 0)


  # Fit model

  dta <-
    dta %>%
    dplyr::mutate(
      estimates =
        purrr::map(
          .x = dplyr::row_number(),
          .f = function(x){
            fit_model(
              data = wiki,
              init = dta$init[x],
              final = dta$final[x],
              party = dta$party[x],
              alpha_init = dta$alpha_init[x],
              alpha_final = dta$alpha_final[x]
            )
          }
        )
    )


  # Now, we'll rotate everything to wide-format and rename some variables.

  dta <-
    do.call("rbind.data.frame", dta$estimates) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      names_from = party,
      values_from = c(est, sd)
    ) %>%
    dplyr::select(
      date,
      con_est = est_con,
      con_err = sd_con,
      lab_est = est_lab,
      lab_err = sd_lab,
      lib_est = est_lib,
      lib_err = sd_lib
    )



  # Then we'll add variable labels

  labelled::var_label(dta) <-
    list(
      date = "Date",
      con_est = "Posterior mean: Conservative voting intention",
      con_err = "Posterior error: Conservative voting intention",
      lab_est = "Posterior mean: Labour voting intention",
      lab_err = "Posterior error: Labour voting intention",
      lib_est = "Posterior mean: Liberal voting intention",
      lib_err = "Posterior error: Liberal voting intention"
    )


  # Update pollbasepro

  pollbasepro <-
    rbind(
      britpol::pollbasepro[britpol::pollbasepro$date < "2019-12-12", ],
      dta
    )


  # Save data

  usethis::use_data(
    pollbasepro,
    internal = FALSE,
    overwrite = TRUE
  )


  # Finally, we'll update the replication information

  save_info(here::here("sessions", "004_pollbasepro.txt"))


}


#' Create Twitter images and text
#'
#' This function calls the latest pollbasepro data and uses it to create and save social media content.
#'
#' @param data Instance of pollbasepro to use to create your output.
#' @param path The file path to where you would like to save your content.

create_twitter_content <- function(data = britpol::pollbasepro, path = NULL){

  # Define party colours

  pty_cols <-
    c(
      "Conservative Party" = "#0087dc",
      "Labour Party" = "#d50000",
      "Liberals (Various Forms)" = "#fdbb30"
    )


  # Create twitter plot

  plot <-
    data %>%
    tidyr::pivot_longer(
      cols = -date,
      names_to = c("party", ".value"),
      names_sep = "_",
    ) %>%
    dplyr::mutate(
      party =
        dplyr::case_when(
          party == "con" ~ "Conservative Party",
          party == "lab" ~ "Labour Party",
          party == "lib" ~ "Liberals (Various Forms)"
        ) %>%
        factor(
          levels =
            c("Conservative Party",
              "Labour Party",
              "Liberals (Various Forms)"
            )
        )
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = date,
        y = est,
        ymin = est - stats::qnorm(0.975)*err,
        ymax = est + stats::qnorm(0.975)*err,
        colour = party,
        fill = party
      )
    ) +
    ggplot2::geom_ribbon(alpha = .3, colour = NA) +
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(values = pty_cols) +
    ggplot2::scale_fill_manual(values = pty_cols) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, .6, by = .1),
      labels = scales::percent_format(accuracy = 1)
    ) +
    ggplot2::scale_x_date(
      breaks = seq.Date(max(data$date) - 365, max(data$date), by = "months"),
      labels = format(seq.Date(max(data$date) - 365, max(data$date), by = "months"), "%b %y")
    ) +
    ggplot2::coord_cartesian(
      ylim = c(0, 0.62),
      xlim = c(max(data$date) - 365, max(data$date))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      text = ggplot2::element_text(family = "Cabin", color = "black", size = 8),
      plot.title = ggplot2::element_text(family = "Cabin", face = "bold", size = ggplot2::rel(1.4), hjust = 0),
      plot.subtitle = ggplot2::element_text(family = "Cabin", size = ggplot2::rel(1), hjust = 0, margin = ggplot2::margin(b = 10)),
      axis.line = ggplot2::element_line(lineend = "round"),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(color = "black", size = ggplot2::rel(1)),
      axis.ticks.x = ggplot2::element_line(lineend = "round"),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(color = "black", size = ggplot2::rel(1)),
      strip.text = ggplot2::element_text(family = "Cabin", face = "bold", size = ggplot2::rel(1)),
      panel.spacing = ggplot2::unit(.3, "cm"),
      panel.grid.major.y = ggplot2::element_line(size = .5, lineend = "round"),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title =
        paste0(
          stringr::str_remove(format(Sys.Date(), "%d %B %Y"), "^0"), ": ",
          ifelse(which.max(c(data$con_est[data$date == max(data$date)], data$lab_est[data$date == max(data$date)])) == 1, "Conservatives ", "Labour "),
          ifelse(which.max(c(data$con_est[data$date == max(data$date)], data$lab_est[data$date == max(data$date)])) == 1, round((data$con_est[data$date == max(data$date)] - data$lab_est[data$date == max(data$date)])*100, 0), round((data$lab_est[data$date == max(data$date)] - data$con_est[data$date == max(data$date)])*100, 0)),
          " Points Ahead"
        ),
      caption = "@PoliSciJack"
    )


  # Save plot

  ggplot2::ggsave(
    plot = plot,
    filename = "twitter.png",
    path = ifelse(is.null(path) == T, here::here(), path),
    device = "png",
    width = (1200*1.5)/320,
    height = (675*1.5)/320,
    units = "in",
    dpi = 320
  )


  # Create Twitter text

  txt_data <-
    data[data$date == max(data$date), ] %>%
    tidyr::pivot_longer(
      cols = -date,
      names_to = c("party", ".value"),
      names_sep = "_"
    ) %>%
    dplyr::arrange(dplyr::desc(est)) %>%
    dplyr::mutate(
      party = tools::toTitleCase(party),
      lower = scales::percent(est - stats::qnorm(.975)*err, accuracy = 1),
      upper = scales::percent(est + stats::qnorm(.975)*err, accuracy = 1),
      share = scales::percent(est, accuracy = 1)
    )

  sink(ifelse(is.null(path) == T, paste0(here::here(), "/tweet.txt"), paste0(path, "/tweet.txt")))
  cat(
    paste("British Poll of Polls,", stringr::str_remove(format(max(data$date), "%d %B %Y"), "^0")),
    paste0(
      "\n\n",
      txt_data$party[1], " lead of ", scales::percent(txt_data$est[1] - txt_data$est[2]), "\n\n",
      txt_data$party[1], ": ", txt_data$share[1], " (", txt_data$lower[1], "-", txt_data$upper[1], ")\n",
      txt_data$party[2], ": ", txt_data$share[2], " (", txt_data$lower[2], "-", txt_data$upper[2], ")\n",
      txt_data$party[3], ": ", txt_data$share[3], " (", txt_data$lower[3], "-", txt_data$upper[3], ")\n"
    )
  )
  sink()

}

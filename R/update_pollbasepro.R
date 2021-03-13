#' Update Voting Intention Polls, 2019-Present
#'
#' This function updates PollBasePro to include the latest polling data and outputs Twitter content.
#'
#'
#' @export

update_pollbasepro <- function(){

  # Get pipe from Magrittr

  `%>%` <- magrittr::`%>%`


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
      alpha_init = PollBasePro::get_result(election = init, party = party),
      alpha_final = NA,
      estimates = as.list(rep(NA, dplyr::n()))
    )


  # Get wiki data and mutate

  wiki <-
    PollBasePro::get_new_polls() %>%
    dplyr::mutate(
      election = "2019-12-12",
      id =
        paste0(
          "poll-",
          seq(
            as.numeric(
              stringr::str_remove(
                PollBasePro::pollbase$id[PollBasePro::pollbase$end > "2019-12-12"][1], ".*-"
              )
            ),
            as.numeric(
              stringr::str_remove(
                PollBasePro::pollbase$id[PollBasePro::pollbase$end > "2019-12-12"][1], ".*-"
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
      election,
      date,
      pollster,
      n,
      con,
      lab,
      lib,
      index
    )


  # Fit model

  dta <-
    dta %>%
    dplyr::mutate(
      estimates =
        purrr::map(
          .x = dplyr::row_number(),
          .f = function(x){
            PollBasePro::fit_model(
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
    dplyr::mutate(
      election =
        date %>%
        PollBasePro::get_last_election()
    ) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      names_from = party,
      values_from = c(est, sd)
    ) %>%
    dplyr::select(
      date,
      election,
      con_est = est_con,
      con_err = sd_con,
      lab_est = est_lab,
      lab_err = sd_lab,
      lib_est = est_lib,
      lib_err = sd_lib
    )


  # We'll also add an indicator to show which party controlled the government

  dta <-
    dta %>%
    dplyr::mutate(
      govt =
        dplyr::case_when(
          election == lubridate::as_date("1955-05-26") ~ 1,
          election == lubridate::as_date("1959-10-08") ~ 1,
          election == lubridate::as_date("1964-10-15") ~ 2,
          election == lubridate::as_date("1966-03-31") ~ 2,
          election == lubridate::as_date("1970-06-18") ~ 1,
          election == lubridate::as_date("1974-02-28") ~ 2,
          election == lubridate::as_date("1974-10-10") ~ 2,
          election == lubridate::as_date("1979-05-03") ~ 1,
          election == lubridate::as_date("1983-06-09") ~ 1,
          election == lubridate::as_date("1987-06-11") ~ 1,
          election == lubridate::as_date("1992-04-09") ~ 1,
          election == lubridate::as_date("1997-05-01") ~ 2,
          election == lubridate::as_date("2001-06-07") ~ 2,
          election == lubridate::as_date("2005-05-05") ~ 2,
          election == lubridate::as_date("2010-05-06") ~ 1,
          election == lubridate::as_date("2015-05-07") ~ 1,
          election == lubridate::as_date("2017-06-08") ~ 1,
          election == lubridate::as_date("2019-12-12") ~ 1
        ) %>%
        haven::labelled(labels = c(Conservative = 1, Labour = 2))
    ) %>%
    dplyr::relocate(govt, .before = "con_est")


  # Plus we'll also add indicators that show who led each party at each timepoint

  dta <-
    dta %>%
    dplyr::mutate(
      con_ldr =
        dplyr::case_when(
          date >= lubridate::as_date("1955-04-21") & date < lubridate::as_date("1957-01-22") ~ 1,
          date >= lubridate::as_date("1957-01-22") & date < lubridate::as_date("1963-11-11") ~ 2,
          date >= lubridate::as_date("1963-11-11") & date < lubridate::as_date("1965-07-27") ~ 3,
          date >= lubridate::as_date("1965-07-27") & date < lubridate::as_date("1975-02-11") ~ 4,
          date >= lubridate::as_date("1975-02-11") & date < lubridate::as_date("1990-11-27") ~ 5,
          date >= lubridate::as_date("1990-11-27") & date < lubridate::as_date("1997-06-19") ~ 6,
          date >= lubridate::as_date("1997-06-19") & date < lubridate::as_date("2001-09-13") ~ 7,
          date >= lubridate::as_date("2001-09-13") & date < lubridate::as_date("2003-11-06") ~ 8,
          date >= lubridate::as_date("2003-11-06") & date < lubridate::as_date("2005-12-06") ~ 9,
          date >= lubridate::as_date("2005-12-06") & date < lubridate::as_date("2016-07-11") ~ 10,
          date >= lubridate::as_date("2016-07-11") & date < lubridate::as_date("2019-07-23") ~ 11,
          date >= lubridate::as_date("2019-07-23") ~ 12
        ) %>%
        haven::labelled(
          labels =
            c(
              `Anthony Eden` = 1,
              `Harold Macmillan` = 2,
              `Alec Douglas-Home` = 3,
              `Edward Heath` = 4,
              `Margaret Thatcher` = 5,
              `John Major` = 6,
              `William Hague` = 7,
              `Iain Duncan Smith` = 8,
              `Michael Howard` = 9,
              `David Cameron` = 10,
              `Theresa May` = 11,
              `Boris Johnson` = 12
            )
        ),
      lab_ldr =
        dplyr::case_when(
          date >= lubridate::as_date("1935-10-08") & date < lubridate::as_date("1955-12-07") ~ 1,
          date >= lubridate::as_date("1955-12-07") & date < lubridate::as_date("1955-12-14") ~ 2,
          date >= lubridate::as_date("1955-12-14") & date < lubridate::as_date("1963-01-18") ~ 3,
          date >= lubridate::as_date("1963-01-18") & date < lubridate::as_date("1963-02-14") ~ 4,
          date >= lubridate::as_date("1963-02-14") & date < lubridate::as_date("1976-04-05") ~ 5,
          date >= lubridate::as_date("1976-04-05") & date < lubridate::as_date("1980-11-10") ~ 6,
          date >= lubridate::as_date("1980-11-10") & date < lubridate::as_date("1983-10-02") ~ 7,
          date >= lubridate::as_date("1983-10-02") & date < lubridate::as_date("1992-07-18") ~ 8,
          date >= lubridate::as_date("1992-07-18") & date < lubridate::as_date("1994-05-12") ~ 9,
          date >= lubridate::as_date("1994-05-12") & date < lubridate::as_date("1994-07-21") ~ 10,
          date >= lubridate::as_date("1994-07-21") & date < lubridate::as_date("2007-06-24") ~ 11,
          date >= lubridate::as_date("2007-06-24") & date < lubridate::as_date("2010-05-11") ~ 12,
          date >= lubridate::as_date("2010-05-11") & date < lubridate::as_date("2010-09-25") ~ 13,
          date >= lubridate::as_date("2010-09-25") & date < lubridate::as_date("2015-05-08") ~ 14,
          date >= lubridate::as_date("2015-05-08") & date < lubridate::as_date("2015-09-12") ~ 13,
          date >= lubridate::as_date("2015-09-12") & date < lubridate::as_date("2020-04-04") ~ 15,
          date >= lubridate::as_date("2020-04-04") ~ 16
        ) %>%
        haven::labelled(
          labels =
            c(
              `Clement Attlee` = 1,
              `Herbert Morrison` = 2,
              `Hugh Gaitskell` = 3,
              `George Brown` = 4,
              `Harold Wilson` = 5,
              `James Callaghan` = 6,
              `Michael Foot` = 7,
              `Neil Kinnock` = 8,
              `John Smith` = 9,
              `Margaret Beckett` = 10,
              `Tony Blair` = 11,
              `Gordon Brown` = 12,
              `Harriet Harman` = 13,
              `Ed Miliband` = 14,
              `Jeremy Corbyn` = 15,
              `Keir Starmer` = 16
            )
        ),
      lib_ldr =
        dplyr::case_when(
          date >= lubridate::as_date("1945-08-02") & date < lubridate::as_date("1956-11-05") ~ 1,
          date >= lubridate::as_date("1956-11-05") & date < lubridate::as_date("1967-01-18") ~ 2,
          date >= lubridate::as_date("1967-01-18") & date < lubridate::as_date("1976-05-12") ~ 3,
          date >= lubridate::as_date("1976-05-12") & date < lubridate::as_date("1976-07-07") ~ 2,
          date >= lubridate::as_date("1976-07-07") & date < lubridate::as_date("1988-03-03") ~ 4,
          date >= lubridate::as_date("1988-03-03") & date < lubridate::as_date("1988-07-16") ~ 5,
          date >= lubridate::as_date("1988-07-16") & date < lubridate::as_date("1999-08-09") ~ 6,
          date >= lubridate::as_date("1999-08-09") & date < lubridate::as_date("2006-01-07") ~ 7,
          date >= lubridate::as_date("2006-01-07") & date < lubridate::as_date("2007-10-15") ~ 8,
          date >= lubridate::as_date("2007-10-15") & date < lubridate::as_date("2007-12-18") ~ 9,
          date >= lubridate::as_date("2007-12-18") & date < lubridate::as_date("2015-07-16") ~ 10,
          date >= lubridate::as_date("2015-07-16") & date < lubridate::as_date("2017-07-20") ~ 11,
          date >= lubridate::as_date("2017-07-20") & date < lubridate::as_date("2019-07-22") ~ 9,
          date >= lubridate::as_date("2019-07-22") & date < lubridate::as_date("2019-12-13") ~ 12,
          date >= lubridate::as_date("2019-12-13") & date < lubridate::as_date("2020-01-01") ~ 13,
          date >= lubridate::as_date("2020-01-01") & date < lubridate::as_date("2020-08-27") ~ 14,
          date >= lubridate::as_date("2020-08-27") ~ 15
        ) %>%
        haven::labelled(
          labels =
            c(
              `Clement Davies` = 1,
              `Jo Grimond` = 2,
              `Jeremy Thorpe` = 3,
              `David Steel` = 4,
              `David Steel & Bob Maclennan` = 5,
              `Paddy Ashdown` = 6,
              `Charles Kennedy` = 7,
              `Menzies Campbell` = 8,
              `Vince Cable` = 9,
              `Nick Clegg` = 10,
              `Tim Farron` = 11,
              `Jo Swinson` = 12,
              `Ed Davey & Sal Brinton` = 13,
              `Ed Davey & Mark Pack` = 14,
              `Ed Davey` = 15
            )
        )
    )


  # And we'll also add indicators that allow users to subset the data
  # to weekly, monthly, and quarterly series with ease.

  dta <-
    dta %>%
    dplyr::mutate(
      week = ifelse(date == (lubridate::ceiling_date(date, "week") - 1), 1, 0),
      month = ifelse(date == (lubridate::ceiling_date(date, "month") - 1), 1, 0),
      quarter = ifelse(date == (lubridate::ceiling_date(date, "quarter") - 1), 1, 0),
      year = ifelse(date == (lubridate::ceiling_date(date, "year") - 1), 1, 0)
    )



  # Then we'll add variable labels

  labelled::var_label(dta) <-
    list(
      date = "Date",
      election = "Date of last general election",
      govt = "Largest party in government after the last  general election",
      con_est = "Posterior mean: Conservative voting intention",
      con_err = "Posterior error: Conservative voting intention",
      lab_est = "Posterior mean: Labour voting intention",
      lab_err = "Posterior error: Labour voting intention",
      lib_est = "Posterior mean: Liberal voting intention",
      lib_err = "Posterior error: Liberal voting intention",
      con_ldr = "Leader of the Conservative Party",
      lab_ldr = "Leader of the Labour Party",
      lib_ldr = "Leader of the Liberals (various forms)",
      week = "Weekly subset indicator",
      month = "Monthly subset indicator",
      quarter = "Quarterly subset indicator",
      year = "Yearly subset indicator"
    )


  # Define party colours

  pty_cols <-
    c(
      "Conservative Party" = "#0087dc",
      "Labour Party" = "#d50000",
      "Liberals (Various Forms)" = "#fdbb30"
    )


  # Create twitter plot

  plot <-
    dta %>%
    tidyr::pivot_longer(
      cols = c(-date, -election, -govt, -dplyr::matches("_ldr"), -week, -month, -quarter, -year),
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
      breaks = seq.Date(max(dta$date) - 365, max(dta$date), by = "months"),
      labels = format(seq.Date(max(dta$date) - 365, max(dta$date), by = "months"), "%b %y")
    ) +
    ggplot2::coord_cartesian(
      ylim = c(0, 0.62),
      xlim = c(max(dta$date) - 365, max(dta$date))
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
          ifelse(which.max(c(dta$con_est[dta$date == max(dta$date)], dta$lab_est[dta$date == max(dta$date)])) == 1, "Conservatives ", "Labour "),
          ifelse(which.max(c(dta$con_est[dta$date == max(dta$date)], dta$lab_est[dta$date == max(dta$date)])) == 1, round((dta$con_est[dta$date == max(dta$date)] - dta$lab_est[dta$date == max(dta$date)])*100, 0), round((dta$lab_est[dta$date == max(dta$date)] - dta$con_est[dta$date == max(dta$date)])*100, 0)),
          " Points Ahead"
        ),
      caption = "@PoliSciJack"
    )


  # Save plot

  ggplot2::ggsave(
    plot = plot,
    filename = "twitter.png",
    path = here::here("download"),
    device = "png",
    width = (1200*1.5)/320,
    height = (675*1.5)/320,
    units = "in",
    dpi = 320
  )


  # Create Twitter text

  txt_dta <-
    dta[dta$date == max(dta$date), ] %>%
    tidyr::pivot_longer(
      cols = c(-date, -election, -govt, -dplyr::matches("_ldr"), -week, -month, -quarter, -year),
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

  sink(here::here("download", "tweet.txt"))
  cat(
    paste("British Poll of Polls,", stringr::str_remove(format(max(dta$date), "%d %B %Y"), "^0")),
    paste0(
      "\n\n",
      txt_dta$party[1], " lead of ", scales::percent(txt_dta$est[1] - txt_dta$est[2]), "\n\n",
      txt_dta$party[1], ": ", txt_dta$share[1], " (", txt_dta$lower[1], "-", txt_dta$upper[1], ")\n",
      txt_dta$party[2], ": ", txt_dta$share[2], " (", txt_dta$lower[2], "-", txt_dta$upper[2], ")\n",
      txt_dta$party[3], ": ", txt_dta$share[3], " (", txt_dta$lower[3], "-", txt_dta$upper[3], ")\n"
    )
  )
  sink()


  # Update pollbasepro

  pollbasepro <-
    rbind(
      PollBasePro::pollbasepro[PollBasePro::pollbasepro$date < "2019-12-12", ],
      dta
    )


  # Save data

  usethis::use_data(
    pollbasepro,
    internal = FALSE,
    overwrite = TRUE
  )


  # And we'll also save the file as a .dta and .sav file for easy use

  haven::write_dta(
    pollbasepro,
    path = here::here("download", paste0("pollbasepro_", utils::packageVersion("PollBasePro"), ".dta"))
  )

  haven::write_sav(
    pollbasepro,
    path = here::here("download", paste0("pollbasepro_", utils::packageVersion("PollBasePro"), ".sav")),
    compress = T
  )


  # Finally, we'll update the replication information

  PollBasePro::save_info(here::here("sessions", "003_pollbasepro.txt"))


}


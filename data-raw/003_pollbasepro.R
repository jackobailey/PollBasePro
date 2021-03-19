
# Fit Model to Data


# 1. Housekeeping ---------------------------------------------------------


# Load packages

library(britpol)
library(tidyverse)
library(lubridate)
library(labelled)
library(magrittr)
library(cmdstanr)
library(haven)
library(here)


# Load pollbase data

data("pollbase")


# Load imputed sample sizes

load(here("R", "sysdata.rda"))


# Get election dates

elec_dates <-
  c(
    "1955-05-26",
    "1959-10-08",
    "1964-10-15",
    "1966-03-31",
    "1970-06-18",
    "1974-02-28",
    "1974-10-10",
    "1979-05-03",
    "1983-06-09",
    "1987-06-11",
    "1992-04-09",
    "1997-05-01",
    "2001-06-07",
    "2005-05-05",
    "2010-05-06",
    "2015-05-07",
    "2017-06-08",
    "2019-12-12"
  )



# 2. Transform data -------------------------------------------------------

# Before we fit the model to estimate daily voting intention estimates, we
# first need to transform the raw pollbase data. The first thing that we
# need to do is to count how many days each poll was in the field duplicate
# it that many times, and then record the number of field days.

pollbase <-
  pollbase %>%
  mutate(
    days =
      interval(start, end) %>%
      divide_by(days(1)) %>%
      add(1)
  ) %>%
  uncount(days) %>%
  group_by(id) %>%
  mutate(
    date =
      start %m+%
      days(row_number() - 1),
    days = n()
  ) %>%
  ungroup() %>%
  select(-start, -end) %>%
  relocate(date, .before = "pollster") %>%
  relocate(days, .before = "election")


# Next, we need to add an index variable that counts the days that have
# passed since the last election took place

pollbase <-
  pollbase %>%
  mutate(
    index =
      interval(election, date) %>%
      divide_by(days(1)) %>%
      add(1)
  )


# Next, we'll remove any polls that took place on the day of an election,
# as we have perfect information on these days

pollbase <-
  pollbase %>%
  filter(!(date %in% as_date(elec_dates)))


# Finally, we'll merge in the imputed sample sizes that we estimated using
# the timeline data.

pollbase <-
  left_join(
    pollbase,
    samplesizes,
    by = "date"
  )



# 3. Fit model ------------------------------------------------------------

# We're going to fit the model separately for each election and each party.
# As such, we'll create an empty tibble that we can use map() to populate.
# We'll provide estimates from the 1955 election onwards, as the elections
# before do not have enough polls at their beginnings.

pollbasepro <-
  tibble(
    init = elec_dates,
    con = NA,
    lab = NA,
    lib = NA
  ) %>%
  mutate(final = c(init[2:n()], NA)) %>%
  pivot_longer(
    cols = c(con, lab, lib),
    names_to = "party",
    values_to = "estimates"
  ) %>%
  mutate(estimates = as.list(rep(NA, n())))


# Next, we need to add the election results for the initial and final
# elections for each party, for each election. Likewise, we also need
# to add empty vectors for the pollster effects.

pollbasepro <-
  pollbasepro %>%
  mutate(
    alpha_init = get_result(election = init, party = party),
    alpha_final = get_result(election = final, party = party)
  )


# Now, we'll loop over the data and fit each model in turn

pollbasepro <-
  pollbasepro %>%
  mutate(
    estimates =
      map(
        .x = row_number(),
        .f = function(x){
          fit_model(
            data = pollbase,
            init = pollbasepro$init[x],
            final = pollbasepro$final[x],
            party = pollbasepro$party[x],
            alpha_init = pollbasepro$alpha_init[x],
            alpha_final = pollbasepro$alpha_final[x]
          )
        }
      )
  )


# Now, we'll rotate everything to wide-format and rename some variables.

pollbasepro <-
  do.call("rbind.data.frame", pollbasepro$estimates) %>%
  mutate(
    election =
      date %>%
      get_last_election()
  ) %>%
  distinct() %>%
  pivot_wider(
    names_from = party,
    values_from = c(est, sd)
  ) %>%
  select(
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

pollbasepro <-
  pollbasepro %>%
  mutate(
    govt =
      case_when(
        election == as_date("1955-05-26") ~ 1,
        election == as_date("1959-10-08") ~ 1,
        election == as_date("1964-10-15") ~ 2,
        election == as_date("1966-03-31") ~ 2,
        election == as_date("1970-06-18") ~ 1,
        election == as_date("1974-02-28") ~ 2,
        election == as_date("1974-10-10") ~ 2,
        election == as_date("1979-05-03") ~ 1,
        election == as_date("1983-06-09") ~ 1,
        election == as_date("1987-06-11") ~ 1,
        election == as_date("1992-04-09") ~ 1,
        election == as_date("1997-05-01") ~ 2,
        election == as_date("2001-06-07") ~ 2,
        election == as_date("2005-05-05") ~ 2,
        election == as_date("2010-05-06") ~ 1,
        election == as_date("2015-05-07") ~ 1,
        election == as_date("2017-06-08") ~ 1,
        election == as_date("2019-12-12") ~ 1
      ) %>%
      labelled(labels = c(Conservative = 1, Labour = 2))
  ) %>%
  relocate(govt, .before = "con_est")


# Plus we'll also add indicators that show who led each party at each timepoint

pollbasepro <-
  pollbasepro %>%
  mutate(
    con_ldr =
      case_when(
        date >= as_date("1955-04-21") & date < as_date("1957-01-22") ~ 1,
        date >= as_date("1957-01-22") & date < as_date("1963-11-11") ~ 2,
        date >= as_date("1963-11-11") & date < as_date("1965-07-27") ~ 3,
        date >= as_date("1965-07-27") & date < as_date("1975-02-11") ~ 4,
        date >= as_date("1975-02-11") & date < as_date("1990-11-27") ~ 5,
        date >= as_date("1990-11-27") & date < as_date("1997-06-19") ~ 6,
        date >= as_date("1997-06-19") & date < as_date("2001-09-13") ~ 7,
        date >= as_date("2001-09-13") & date < as_date("2003-11-06") ~ 8,
        date >= as_date("2003-11-06") & date < as_date("2005-12-06") ~ 9,
        date >= as_date("2005-12-06") & date < as_date("2016-07-11") ~ 10,
        date >= as_date("2016-07-11") & date < as_date("2019-07-23") ~ 11,
        date >= as_date("2019-07-23") ~ 12
      ) %>%
      labelled(
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
      case_when(
        date >= as_date("1935-10-08") & date < as_date("1955-12-07") ~ 1,
        date >= as_date("1955-12-07") & date < as_date("1955-12-14") ~ 2,
        date >= as_date("1955-12-14") & date < as_date("1963-01-18") ~ 3,
        date >= as_date("1963-01-18") & date < as_date("1963-02-14") ~ 4,
        date >= as_date("1963-02-14") & date < as_date("1976-04-05") ~ 5,
        date >= as_date("1976-04-05") & date < as_date("1980-11-10") ~ 6,
        date >= as_date("1980-11-10") & date < as_date("1983-10-02") ~ 7,
        date >= as_date("1983-10-02") & date < as_date("1992-07-18") ~ 8,
        date >= as_date("1992-07-18") & date < as_date("1994-05-12") ~ 9,
        date >= as_date("1994-05-12") & date < as_date("1994-07-21") ~ 10,
        date >= as_date("1994-07-21") & date < as_date("2007-06-24") ~ 11,
        date >= as_date("2007-06-24") & date < as_date("2010-05-11") ~ 12,
        date >= as_date("2010-05-11") & date < as_date("2010-09-25") ~ 13,
        date >= as_date("2010-09-25") & date < as_date("2015-05-08") ~ 14,
        date >= as_date("2015-05-08") & date < as_date("2015-09-12") ~ 13,
        date >= as_date("2015-09-12") & date < as_date("2020-04-04") ~ 15,
        date >= as_date("2020-04-04") ~ 16
      ) %>%
      labelled(
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
      case_when(
        date >= as_date("1945-08-02") & date < as_date("1956-11-05") ~ 1,
        date >= as_date("1956-11-05") & date < as_date("1967-01-18") ~ 2,
        date >= as_date("1967-01-18") & date < as_date("1976-05-12") ~ 3,
        date >= as_date("1976-05-12") & date < as_date("1976-07-07") ~ 2,
        date >= as_date("1976-07-07") & date < as_date("1988-03-03") ~ 4,
        date >= as_date("1988-03-03") & date < as_date("1988-07-16") ~ 5,
        date >= as_date("1988-07-16") & date < as_date("1999-08-09") ~ 6,
        date >= as_date("1999-08-09") & date < as_date("2006-01-07") ~ 7,
        date >= as_date("2006-01-07") & date < as_date("2007-10-15") ~ 8,
        date >= as_date("2007-10-15") & date < as_date("2007-12-18") ~ 9,
        date >= as_date("2007-12-18") & date < as_date("2015-07-16") ~ 10,
        date >= as_date("2015-07-16") & date < as_date("2017-07-20") ~ 11,
        date >= as_date("2017-07-20") & date < as_date("2019-07-22") ~ 9,
        date >= as_date("2019-07-22") & date < as_date("2019-12-13") ~ 12,
        date >= as_date("2019-12-13") & date < as_date("2020-01-01") ~ 13,
        date >= as_date("2020-01-01") & date < as_date("2020-08-27") ~ 14,
        date >= as_date("2020-08-27") ~ 15
      ) %>%
      labelled(
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

pollbasepro <-
  pollbasepro %>%
  mutate(
    week = ifelse(date == (ceiling_date(date, "week") - 1), 1, 0),
    month = ifelse(date == (ceiling_date(date, "month") - 1), 1, 0),
    quarter = ifelse(date == (ceiling_date(date, "quarter") - 1), 1, 0),
    year = ifelse(date == (ceiling_date(date, "year") - 1), 1, 0)
  )



# Then we'll add variable labels

var_label(pollbasepro) <-
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



# 4. Save data to disk ----------------------------------------------------

# Then we'll save the data to disk

usethis::use_data(
  pollbasepro,
  internal = FALSE,
  overwrite = TRUE
)


# Finally, we'll install and restart the package so that subsequent scripts
# call the most recent data.

devtools::install(upgrade = "never")



# 5. Produce replication data ---------------------------------------------

# Save system data to the "sessions" folder for the sake of transparency and
# future replication.

save_info(path = here("sessions", "003_pollbasepro.txt"))



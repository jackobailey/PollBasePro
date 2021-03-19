
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
  distinct() %>%
  pivot_wider(
    names_from = party,
    values_from = c(est, sd)
  ) %>%
  select(
    date,
    con_est = est_con,
    con_err = sd_con,
    lab_est = est_lab,
    lab_err = sd_lab,
    lib_est = est_lib,
    lib_err = sd_lib
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



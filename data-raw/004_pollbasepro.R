
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

data("samplesizes")



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
  relocate(date, .before = "pollster")


# Next, we need to add an index variable that counts the days that have
# passed since the last election took place

pollbase <-
  pollbase %>%
  add_elections(which = "both") %>%
  mutate(
    index =
      interval(last_elec, date) %>%
      divide_by(days(1)) %>%
      add(1)
  )


# Next, we'll remove any polls that took place on the day of an election,
# as we have perfect information on these days

pollbase <-
  pollbase %>%
  filter(
    date != last_elec,
    date != next_elec
  )


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
# before do not have enough polls at their beginnings. First, let's use the
# election_results data set to compute them

election_results <-
  election_results %>%
  filter(
    !region %in% c("University", "Northern Ireland"),
    sum(c(con_votes, lab_votes, lib_votes, nat_votes, oth_votes), na.rm = T) >0
  ) %>%
  group_by(date) %>%
  summarise(
    total = sum(c(con_votes, lab_votes, lib_votes, nat_votes, oth_votes), na.rm = T),
    con = sum(con_votes, na.rm = T)/total,
    lab = sum(lab_votes, na.rm = T)/total,
    lib = sum(lib_votes, na.rm = T)/total
  ) %>%
  pivot_longer(
    cols = c(con, lab, lib),
    names_to = "party",
    values_to = "results"
  )


# Now let's create our data set

pollbasepro <-
  tibble(
    init = election_dates$date[election_dates$date > "1955-01-01"],
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
    alpha_init = election_results$results[election_results$date > "1955-01-01"],
    alpha_final = lead(election_results$results[election_results$date > "1955-01-01"], 3)
  )


# Now, we'll loop over the data and fit each model in turn

pollbasepro <-
  pollbasepro %>%
  mutate(
    estimates =
      map(
        .x = row_number(),
        .f = function(x){
          britpol:::fit_model(
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



# Then we'll add variable labels

var_label(pollbasepro) <-
  list(
    date = "Date",
    con_est = "Posterior mean: Conservative voting intention",
    con_err = "Posterior error: Conservative voting intention",
    lab_est = "Posterior mean: Labour voting intention",
    lab_err = "Posterior error: Labour voting intention",
    lib_est = "Posterior mean: Liberal voting intention",
    lib_err = "Posterior error: Liberal voting intention"
  )



# 4. Save data to disk ----------------------------------------------------

# Then we'll save the data to disk

usethis::use_data(
  pollbasepro,
  internal = FALSE,
  overwrite = TRUE
)



# 5. Produce replication data ---------------------------------------------

# Save system data to the "sessions" folder for the sake of transparency and
# future replication.

britpol:::save_info(path = here("sessions", "004_pollbasepro.txt"))



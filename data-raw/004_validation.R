
# Validate data


# 1. Housekeeping ---------------------------------------------------------

# Load packages

library(PollBasePro)
library(tidyverse)
library(lubridate)
library(brms)
library(here)


# Load pollbasepro data

data("pollbasepro")


# Load Timeline data, filter to include only UK cases, and split by party

timeline <-
  load_timeline() %>%
  select(
    date = polldate,
    elecdate,
    country,
    party = partyid,
    vote = poll_
  ) %>%
  filter(country == "United Kingdom") %>%
  mutate(
    vote = vote/100,
    party = case_when(party == 1 ~ "con", party == 2 ~ "lab", party == 3 ~ "lib"),
    polldate = as_date(date)
  ) %>%
  na.omit()

con <-
  timeline %>%
  filter(party == "con") %>%
  left_join(
    pollbasepro,
    c("polldate" = "date")
  ) %>%
  na.omit()

lab <-
  timeline %>%
  filter(party == "lab") %>%
  left_join(
    pollbasepro,
    c("polldate" = "date")
  ) %>%
  na.omit()

lib <-
  timeline %>%
  filter(party == "lib") %>%
  left_join(
    pollbasepro,
    c("polldate" = "date")
  ) %>%
  na.omit()



# 3. Fit correlation models -----------------------------------------------

# We're going to validate the PollBasePro data by calculating the correlation
# between them and another data source: the "Timeline of Elections" data
# from Jennings and Wlezien's (2015). As our estimates include known error,
# we can't just use a run-of-the-mill correlation. Instead, we'll fit some
# simple models in brms to calculate the correlation for us while accounting
# for uncertainty.

cor_con <-
  brm(
    formula =
      bf(vote ~ 1) +
      bf(con_est | se(con_err, sigma = TRUE) ~ 1) +
      set_rescor(rescor = TRUE),
    prior =
      prior(normal(0.4, 0.07), class = "Intercept", resp = "vote") +
      prior(exponential(5), class = "sigma", resp = "vote") +
      prior(normal(0.4, 0.07), class = "Intercept", resp = "conest") +
      prior(exponential(5), class = "sigma", resp = "conest") +
      prior(lkj(2), class = "rescor"),
    data = con,
    backend = "cmdstanr",
    seed = 666,
    chains = 4,
    cores = 4,
    file =
      here(
        "models",
        paste0("cor_con_", packageVersion("PollBasePro"))
      )
  )

cor_lab <-
  brm(
    formula =
      bf(vote ~ 1) +
      bf(lab_est | se(lab_err, sigma = TRUE) ~ 1) +
      set_rescor(rescor = TRUE),
    prior =
      prior(normal(0.4, 0.07), class = "Intercept", resp = "vote") +
      prior(exponential(5), class = "sigma", resp = "vote") +
      prior(normal(0.4, 0.07), class = "Intercept", resp = "labest") +
      prior(exponential(5), class = "sigma", resp = "labest") +
      prior(lkj(2), class = "rescor"),
    data = lab,
    backend = "cmdstanr",
    seed = 666,
    chains = 4,
    cores = 4,
    file =
      here(
        "models",
        paste0("cor_lab_", packageVersion("PollBasePro"))
      )
  )

cor_lib <-
  brm(
    formula =
      bf(vote ~ 1) +
      bf(lib_est | se(lib_err, sigma = TRUE) ~ 1) +
      set_rescor(rescor = TRUE),
    prior =
      prior(normal(0.15, 0.06), class = "Intercept", resp = "vote") +
      prior(exponential(5), class = "sigma", resp = "vote") +
      prior(normal(0.15, 0.06), class = "Intercept", resp = "libest") +
      prior(exponential(5), class = "sigma", resp = "libest") +
      prior(lkj(2), class = "rescor"),
    data = lib,
    backend = "cmdstanr",
    seed = 666,
    chains = 4,
    cores = 4,
    file =
      here(
        "models",
        paste0("cor_lib_", packageVersion("PollBasePro"))
      )
  )


# Finally, we'll also calculate the correlations between the parties, so
# that we can be certain not only that the data are make sense when compared
# to real-world data, but also when compared to each other.

cor_all <-
  brm(
    formula =
      bf(lib_est | se(lib_err, sigma = TRUE) ~ 1) +
      bf(con_est | se(con_err, sigma = TRUE) ~ 1) +
      bf(lab_est | se(lab_err, sigma = TRUE) ~ 1) +
      set_rescor(rescor = TRUE),
    prior =
      prior(normal(0.15, 0.06), class = "Intercept", resp = "libest") +
      prior(exponential(5), class = "sigma", resp = "libest") +
      prior(normal(0.4, 0.07), class = "Intercept", resp = "conest") +
      prior(exponential(5), class = "sigma", resp = "conest") +
      prior(normal(0.4, 0.07), class = "Intercept", resp = "labest") +
      prior(exponential(5), class = "sigma", resp = "labest") +
      prior(lkj(2), class = "rescor"),
    data = pollbasepro,
    backend = "cmdstanr",
    seed = 666,
    chains = 4,
    cores = 4,
    file =
      here(
        "models",
        paste0("cor_all_", packageVersion("PollBasePro"))
      )
  )


# Finally, we'll install and restart the package so that subsequent scripts
# call the most recent data.

devtools::install(upgrade = "never")



# 4. Produce replication data ---------------------------------------------

# Save system data to the "sessions" folder for the sake of transparency and
# future replication.

save_info(path = here("sessions", "004_validation.txt"))



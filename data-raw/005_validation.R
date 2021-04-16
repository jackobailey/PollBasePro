
# Validate data


# 1. Housekeeping ---------------------------------------------------------

# Load packages

library(britpol)
library(tidyverse)
library(lubridate)
library(brms)
library(here)


# Load pollbasepro data

data("pollbasepro")


# Load Timeline data, filter to include only UK cases, and split by party

timeline <-
  britpol:::load_timeline() %>%
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
    family = student(),
    prior =
      prior(normal(0.4, 0.07), class = "Intercept", resp = "vote") +
      prior(exponential(5), class = "sigma", resp = "vote") +
      prior(normal(0.4, 0.07), class = "Intercept", resp = "conest") +
      prior(exponential(5), class = "sigma", resp = "conest") +
      prior(lkj(2), class = "rescor") +
      prior(gamma(2, .1), class = "nu"),
    data = con,
    backend = "cmdstanr",
    seed = 666,
    chains = 4,
    cores = 4,
    thin = 4 # Thin to save memory
  )

cor_lab <-
  brm(
    formula =
      bf(vote ~ 1) +
      bf(lab_est | se(lab_err, sigma = TRUE) ~ 1) +
      set_rescor(rescor = TRUE),
    family = student(),
    prior =
      prior(normal(0.4, 0.07), class = "Intercept", resp = "vote") +
      prior(exponential(5), class = "sigma", resp = "vote") +
      prior(normal(0.4, 0.07), class = "Intercept", resp = "labest") +
      prior(exponential(5), class = "sigma", resp = "labest") +
      prior(lkj(2), class = "rescor") +
      prior(gamma(2, .1), class = "nu"),
    data = lab,
    backend = "cmdstanr",
    seed = 666,
    chains = 4,
    cores = 4,
    thin = 4 # Thin to save memory
  )

cor_lib <-
  brm(
    formula =
      bf(vote ~ 1) +
      bf(lib_est | se(lib_err, sigma = TRUE) ~ 1) +
      set_rescor(rescor = TRUE),
    family = student(),
    prior =
      prior(normal(0.15, 0.06), class = "Intercept", resp = "vote") +
      prior(exponential(5), class = "sigma", resp = "vote") +
      prior(normal(0.15, 0.06), class = "Intercept", resp = "libest") +
      prior(exponential(5), class = "sigma", resp = "libest") +
      prior(lkj(2), class = "rescor") +
      prior(gamma(2, .1), class = "nu"),
    data = lib,
    backend = "cmdstanr",
    seed = 666,
    chains = 4,
    cores = 4,
    thin = 4 # Thin to save memory
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
    family = student(),
    prior =
      prior(normal(0.15, 0.06), class = "Intercept", resp = "libest") +
      prior(exponential(5), class = "sigma", resp = "libest") +
      prior(normal(0.4, 0.07), class = "Intercept", resp = "conest") +
      prior(exponential(5), class = "sigma", resp = "conest") +
      prior(normal(0.4, 0.07), class = "Intercept", resp = "labest") +
      prior(exponential(5), class = "sigma", resp = "labest") +
      prior(lkj(2), class = "rescor") +
      prior(gamma(2, .1), class = "nu"),
    data = pollbasepro,
    backend = "cmdstanr",
    seed = 666,
    chains = 4,
    cores = 4,
    thin = 4 # Thin to save memory
  )


# Let's now combine the residual correlations into a list

cor_mods <-
  list(
    "lib_con" = posterior_samples(cor_all, pars = "rescor__libest__conest")[[1]],
    "lib_lab" = posterior_samples(cor_all, pars = "rescor__libest__labest")[[1]],
    "con_lab" = posterior_samples(cor_all, pars = "rescor__conest__labest")[[1]],
    "tl_con" = posterior_samples(cor_con, pars = "rescor__vote__conest")[[1]],
    "tl_lab" = posterior_samples(cor_lab, pars = "rescor__vote__labest")[[1]],
    "tl_lib" = posterior_samples(cor_lib, pars = "rescor__vote__libest")[[1]],
    "mae_con" = britpol:::mae(cor_con$data$con_est*100, cor_con$data$vote*100),
    "mae_lab" = britpol:::mae(cor_lab$data$lab_est*100, cor_lab$data$vote*100),
    "mae_lib" = britpol:::mae(cor_lib$data$lib_est*100, cor_lib$data$vote*100),
    "rmse_con" = britpol:::rmse(cor_con$data$con_est, cor_con$data$vote),
    "rmse_lab" = britpol:::rmse(cor_lab$data$lab_est, cor_lab$data$vote),
    "rmse_lib" = britpol:::rmse(cor_lib$data$lib_est, cor_lib$data$vote)
  )


# Then we'll save them as system data

usethis::use_data(
  cor_mods,
  internal = TRUE,
  overwrite = TRUE
)



# 4. Produce replication data ---------------------------------------------

# Save system data to the "sessions" folder for the sake of transparency and
# future replication.

britpol:::save_info(path = here("sessions", "005_validation.txt"))




# Applied example


# 1. Housekeeping ---------------------------------------------------------

# Load packages

library(britpol)
library(tidyverse)
library(lubridate)
library(brms)
library(here)


# Load pollbasepro data

data("pollbasepro")


# Merge in coronavirus deaths data

pollbasepro <-
  pollbasepro %>%
  left_join(
    read_csv(here("inst", "extdata", "death_dta.csv")) %>%
      select(
        date,
        deaths = newDeaths28DaysByDeathDate
      ),
    by = "date"
  )



# 2. Transform data -------------------------------------------------------

# We've matched the vaccines and deaths data to the pollbase time series,
# but we need to transform everything a little. First, we'll select the
# variables that we need.

pollbasepro <-
  pollbasepro %>%
  select(
    date,
    con_est,
    con_err,
    deaths
  )


# Next, we'll fill any missing deaths data with zeroes to mark out that
# the coronavirus had not yet occurred.

pollbasepro <-
  pollbasepro %>%
  mutate(deaths = ifelse(is.na(deaths) == T, 0, deaths))


# We'll then convert our data from raw figures to changes over the past
# 4 weeks.

pollbasepro <-
  pollbasepro %>%
  mutate(
    con_est = con_est - lag(con_est, 28),
    con_err = sqrt(con_err^2 + lag(con_err, 28)^2),
    deaths = (deaths - lag(deaths, 28))/100
  )


# Now, we'll filter out all cases pre-2019 election and the one case on
# 2021-03-06 where there is no coronavirus data

pollbasepro <-
  pollbasepro %>%
  filter(
    date >= "2019-12-12",
    date != "2021-03-06"
  )


# Finally, we also need to create a time index variable so that we can deal
# with any time-varying variation not due to death or vaccine rates.

pollbasepro <-
  pollbasepro %>%
  mutate(time = interval(min(date), date)/months(1))



# 3. Fit model ------------------------------------------------------------

# Now that we have transformed our data, we can now fit our model. This is
# a simple linear model, but we'll also include an interaction between the
# number of deaths and the passing of time to test if the effect of deaths
# has changed over time. We'll also save the model to disk so that we can
# call it in the paper.

death_mod <-
  brm(
    formula =
      bf(
        con_est | se(con_err, sigma = TRUE) ~ 1 + time*deaths,
        center = FALSE
      ),
    prior =
      prior(normal(0, 0.1), class = "b", coef = "Intercept") +
      prior(normal(0, 0.1), class = "b") +
      prior(exponential(10), class = "sigma"),
    data = pollbasepro,
    backend = "cmdstanr",
    seed = 666,
    chains = 4,
    cores = 4,
    threads = threading(3),
    file =
      here(
        "documentation",
        "_assets",
        paste0("death_mod_", packageVersion("britpol"))
      ),
    file_refit = "on_change"
  )



# 4. Produce replication data ---------------------------------------------

# Save system data to the "sessions" folder for the sake of transparency and
# future replication.

britpol:::save_info(path = here("sessions", "006_example.txt"))



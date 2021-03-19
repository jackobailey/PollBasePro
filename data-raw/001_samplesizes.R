
# Impute missing sample sizes


# 1. Housekeeping ---------------------------------------------------------

# Load packages

library(britpol)
library(tidyverse)
library(lubridate)
library(brms)
library(here)


# Load timeline of elections data

timeline <-
  britpol:::load_timeline() %>%
  select(
    polldate,
    elecdate,
    country,
    sample
  ) %>%
  na.omit()



# 2. Transform data -------------------------------------------------------

# The data transformation here is pretty limited. All we need to do is to
# create a timing variable that counts up from some date. We'll count up
# from 1950-01-01 and scale the variable by decade so that it's easier for
# the model to handle.

timeline <-
  timeline %>%
  mutate(
    time = interval(as_date("1950-01-01"), as_date(polldate))/years(10)
  )


# There are a few cases that have absurdly large sample sizes, likely due
# to including MRP polls, etc. We'll make the decision here to remove any
# cases with sample sizes over 10k, which probably don't reflect the sample
# sizes of a typical poll and serve to bias upwards later sample estimates.

timeline <-
  timeline %>%
  filter(sample < 10000)


# Likewise, there are many cases where the sample size seems to have been
# input as 1000 in the absence of any other information. As we can't tell
# which are real and which are not, we'll remove all of them.

timeline <-
  timeline %>%
  filter(
    sample != 1000,
    sample != 0
  )


# We also need to subset the data to include only distinct cases, as each
# is currently repeated three times -- one for each of the UK's three
# main parties.

timeline <-
  timeline %>%
  distinct()


# Finally, there are some countries that have only a handful of cases. These
# cause issues when fitting the model, so we'll also get rid of any that
# have less than 100 cases.

timeline <-
  timeline %>%
  group_by(country) %>%
  add_count() %>%
  ungroup() %>%
  filter(n >= 100) %>%
  select(-n)



# 3. Fit model and impute sample sizes ------------------------------------

# We'll now fit our model. As the sample size data are counts, we'll model
# them as poisson-distributed according to some constant and some spline
# over time. Note also that we include data from countries other than only
# the UK to benefit from partial pooling. This is especially useful given
# that there are no UK-based sample size figures before 1963, but the data
# that we use begin in 1955.

sample_mod <-
  brm(
    formula = bf(sample ~ 1 + time + (1 + time | country), center = F),
    family = poisson(link = "log"),
    prior =
      prior(normal(7, .5), class = "b", coef = "Intercept") +
      prior(normal(0, .1), class = "b") +
      prior(exponential(10), class = "sd") +
      prior(lkj(2), class = "cor"),
    data = timeline,
    seed = 666,
    backend = "cmdstanr",
    cores = 4,
    chains = 4,
    threads = threading(3),
    max_treedepth = 20,
    file = here("models", paste0("sample_mod_", packageVersion("britpol")))
  )


# We can now use the model to predict sample sizes with error for every
# day that PollBase covers which we can then use in estimating PollBasePro.
# Note that we take only the estimate of the mean, as the mean and variance
# of a poisson distribution are the same.

samplesizes <-
  predict(
    sample_mod,
    newdata =
      tibble(
        date = seq.Date(as_date("1955-01-01"), as_date("2013-01-01"), "day"),
        time = interval(as_date("1955-01-01"), date)/years(10),
        country = "United Kingdom"
      ),
    re_formula = NULL
  ) %>%
  data.frame() %>%
  tibble() %>%
  mutate(date = seq.Date(as_date("1955-01-01"), as_date("2013-01-01"), "day")) %>%
  select(
    date,
    n_est = Estimate
  )



# 4. Save data ------------------------------------------------------------

# Now that we've processed the data, we can save it to the disk. This is
# useful as it makes it much quicker to use the data and allows us to do
# so in other applications too.

usethis::use_data(
  samplesizes,
  internal = FALSE,
  overwrite = TRUE
)


# Finally, we'll install and restart the package so that subsequent scripts
# call the most recent data.

devtools::install(upgrade = "never")



# 4. Produce replication data ---------------------------------------------

# Save system data to the "sessions" folder for the sake of transparency and
# future replication.

save_info(path = here("sessions", "001_samplesizes.txt"))



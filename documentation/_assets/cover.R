
# Create user guide cover image


# 1. Housekeeping ---------------------------------------------------------

# Load packages

library(tidyverse)
library(magrittr)
library(ggimage)
library(here)


# Load pollbasepro data

data("pollbasepro")



# 2. Create plot ----------------------------------------------------------

# We're going to create a simple plot to use as the user guide cover. The
# neat thing about it is that we can use randomisation to make the cover be
# different each day. We'll set it up so that the proportion of blue, red,
# and orange graphics on the cover are roughly proportional to the normalised
# vote shares of the Conservatives, Labour, and the Liberals according to
# our most recent estimate.

# First, we'll get today's date and convert to numeric

date_now <- as.numeric(Sys.Date())


# Second, we'll use the date to set the random seed

set.seed(date_now)


# Next, we'll get the most recent estimates from pollbasepro and normalise
# them to sum to 1.

props <-
  pollbasepro %>%
  filter(date == max(date)) %>%
  select(matches("_est")) %>%
  mutate_all(function(x) x/rowSums(.)) %>%
  t() %>%
  rep(5) %>%
  divide_by(5)


# Next, we'll get  icon file paths and shuffle them according to the different
# normalised proportions that we estimated.

paths <- dir(here("documentation", "_assets", "icons"), full.names = T)
paths <- sample(paths, size = 169, replace = T, prob = props)


# Then, we'll generate some basic data

dta <-
  tibble(
    x = rep(seq(0, 6, by = .5), each = 13),
    y = rep(seq(0, 6, by = .5), 13),
    image = paths
  )


# And, finally, we'll generate today's cover plot

cover <-
  dta %>%
  ggplot(
    aes(
      x = x,
      y = y
    )
  ) +
  geom_image(
    aes(
      x = x,
      y = y,
      image = paths
    ),
    size = 0.04,
    by = "width",
    asp = 1
  ) +
  coord_cartesian(
    xlim = c(0, 6),
    ylim = c(0, 6)
  ) +
  theme_void() +
  theme(legend.position = "none")



# 3. Render plot ----------------------------------------------------------

# Now that we've created the plot, we'll save it to disk so that we can call
# it from our Rmarkdown script.

ggsave(
  filename = "cover.png",
  plot = cover,
  path = here("documentation", "_assets"),
  width = 6,
  height = 6,
  units = "in",
  dpi = 320
)


# Compile PollBasePro

# 1. Housekeeping ---------------------------------------------------------

# Load packages

library(PollBasePro)
library(rmarkdown)
library(here)



# 2. Compile data and models ----------------------------------------------

# Fit sample size imputation model

source(here("data-raw", "001_samplesizes.R"))


# Format raw pollbase data

source(here("data-raw", "002_pollbase.R"))


# Derive daily pollbasepro estimates

source(here("data-raw", "003_pollbasepro.R"))


# Validate the resulting estimates

source(here("data-raw", "004_validation.R"))


# Use data to fit applied example model for the paper

source(here("data-raw", "005_example.R"))



# 3. Compile documentation ------------------------------------------------

# Render paper

render(
  input = here("documentation", "paper.Rmd"),
  output_file = here("download", paste0("paper_", packageVersion("PollBasePro"), ".pdf"))
)


# Generate random cover image

source(here("documentation", "_assets", "cover.R"))


# Render user guide and codebook

render(
  input = here("documentation", "userguide.Rmd"),
  output_file = here("download", paste0("userguide_", packageVersion("PollBasePro"), ".pdf"))
)


# Render GitHub README file

render(
  input = here("documentation", "readme.Rmd"),
  output_format = "github_document",
  output_file = here("README.md")
)



# 4. Produce replication materials ----------------------------------------

# Save session information to the "sessions" folder

save_info(path = here("sessions", "000_compile.txt"))


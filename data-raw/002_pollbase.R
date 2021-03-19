
# Compile long-format PollBase data


# 1. Housekeeping ---------------------------------------------------------

# Load packages

library(britpol)
library(tidyverse)
library(magrittr)
library(lubridate)
library(labelled)
library(janitor)
library(readxl)
library(haven)
library(here)


# Get index of first sheet to start with a number

start <-
  here("inst", "extdata", "PollBase-Q4-2020.xlsx") %>%
  excel_sheets() %>%
  str_detect("^[:digit:]") %>%
  which() %>%
  pluck(1)


# Get count of sheets in the raw data that start with a number

sheets <-
  here("inst", "extdata", "PollBase-Q4-2020.xlsx") %>%
  excel_sheets() %>%
  str_detect("^[:digit:]") %>%
  sum()



# Load raw data from Mark Pack's website

pollbase <-
  tibble(n = start:sheets) %>%
  mutate(
    data =
      map(
        .x = n,
        .f = function(x){
          read_xlsx(
            here("inst", "extdata", "PollBase-Q4-2020.xlsx"),
            sheet = x,
            col_types = "text",
            .name_repair = "minimal"
          )
        }
      )
  )


# Load sample size data

data("samplesizes")



# 2. Transform PollBase data ----------------------------------------------

# Before we get started, we'll make our life easier by cleaning the column
# titles for each sheet of data in our list.

pollbase <-
  pollbase %>%
  mutate(
    data =
      map(
        .x = data,
        .f = function(x){
          x %>%
            clean_names() %>%
            select(-matches("^x.*"),
                   -ends_with("lead"),
                   -ends_with("net"),
                   -ends_with("good"),
                   -ends_with("bad"),
                   -ends_with("best_pm"),
                   -ends_with("_38"),
                   -ends_with("_47"),
                   -ends_with("_48"),
                   -ends_with("_49"),
                   -ends_with("_50"),
                   -ends_with("_51"),
                   -matches("question"),
                   -matches("leader"),
                   -matches("^m_$"),
                   -matches("^f_$"),
                   -matches("^net_$"))
        }
      )
  )


# For each data sheet, we'll select only the variables that we need. These
# are the dates, the polling companies, and each party's vote share.

pollbase <-
  pollbase %>%
  mutate(
    data =
      map(
        .x = data,
        .f = function(x){
          x %>%
            select(
              year,
              month,
              days = fieldwork,
              pollster = polling,
              con,
              lab,
              lib = ld
            )
        }
      )
  )


# At present, the year columns only include a marker that tells when each
# year starts. Before we merge the data, we need to fill in the missing
# data for each year and month. We also need to take care of any values
# below 1943, which appear in some cases.

pollbase <-
  pollbase %>%
  mutate(
    data =
      map(
        .x = data,
        .f = function(x){
          x %>%
            mutate(
              month =
                month %>%
                str_remove("\\?"),
              year =
                year %>%
                as.numeric() %>%
                ifelse(. < 1943, NA, .)
            ) %>%
            fill(year) %>%
            fill(month)
        }
      )
  )


# Now we can merge each sheet in our list into a single data file.

pollbase <- do.call(rbind, pollbase$data)


# Next, we'll drop any cases which include NAs on the pollster and party
# variables.

pollbase <-
  pollbase %>%
  drop_na(
    pollster,
    con,
    lab,
    lib
  )


# We'll then drop any exit polls

pollbase <-
  pollbase %>%
  filter(
    !(pollster %in% c("Exit poll", "Result"))
  )


# We'll also remove all hyphens from the pollster variable so that, for
# example, "TNS BMRB" and "TNS-BMRB" aren't treated separately. We'll
# also convert all the names to lower case so that there is no longer a
# difference between "Onepoll" and "OnePoll".

pollbase <-
  pollbase %>%
  mutate(
    pollster =
      pollster %>%
      str_remove_all("-") %>%
      tolower()
  )


# Next, we'll recode some inconsistencies in the month column with the
# name of September (some are Sep, some are Sept). We'll also fix a
# case which includes the month in the days column.

pollbase <-
  pollbase %>%
  mutate(
    days =
      days %>%
      str_remove(" Sept"),
    month =
      month %>%
      str_replace("Sept", "Sep")
  )


# At present, the fieldwork dates have some issues that we need to fix.
# First, two cases have uncertain date ranges. As there's no real way
# of knowing which is the correct one, we'll spread them evenly over
# the entire month.

pollbase <-
  pollbase %>%
  mutate(
    days =
      days %>%
      str_remove("28-2 or 11-16") %>%
      str_remove("11-16 or 5-10")
  )


# Second, there are some cases that are uncertain and that contain a
# question mark. Where these contain only a single number (judging by
# the number of characters) or where they contain two numbers (again
# judging by the number of characters), we'll mark them as NA so that
# they get spread over the entire month.

pollbase <-
  pollbase %>%
  mutate(
    days =
      case_when(
        str_detect(days, "[0-99]-[0-99]\\?") == T ~ NA_character_,
        str_detect(days, "[0-99]\\?") == T ~ NA_character_,
        str_detect(days, "[0-99] \\?") == T ~ NA_character_,
        TRUE ~ days
      )
  )


# Third, there are some cases where the fieldwork variable indicates a
# date range, but only appears to provide the end date. In these cases,
# we'll assume that the first date was the first of the month.

pollbase <-
  pollbase %>%
  mutate(
    days =
      ifelse(
        str_detect(days, "^-") == T,
        paste0(1, days),
        days
      )
  )


# Fourth, we need to deal with any cases with only one date and reformat
# them so that the separate function that we use later doesn't throw an
# error.

pollbase <-
  pollbase %>%
  mutate(
    days =
      ifelse(
        nchar(days) %in% 1:2,
        paste0(days, "-", days),
        days
      )
  )


# Fifth, some of the fieldwork dates are marked as "Exit" because they
# correspond with an exit poll. Where that's the case, we'll delete them.

pollbase <-
  pollbase %>%
  filter(
    !(str_detect(days, "Exit")),
    str_detect(days, "")
  )


# Sixth, there is a single case that uses a slash and not a hyphen and
# another single case that uses a hypen to indicate an uncertain date.
# We'll fix the first and remove the date range from the latter.

pollbase <-
  pollbase %>%
  mutate(days = str_replace(days, "/", "-")) %>%
  mutate(days = ifelse(nchar(days) > 5, NA, days))


# Now we can split the fieldwork dates into a start and end date.

pollbase <-
  pollbase %>%
  separate(
    col = days,
    sep = "-",
    into = c("start", "end")
  )


# We now need to go through and deal with some issues with the "start"
# variable that we've just come up with. This includes removing some
# more uncertain values (e.g. "c.6") and marking question marks as
# NA. We'll then mark any missing dates as 1.

pollbase <-
  pollbase %>%
  mutate(
    start =
      start %>%
      na_if("c.6") %>%
      na_if("?") %>%
      na_if("") %>%
      str_remove("/.*") %>%
      str_remove(" ") %>%
      as.numeric() %>%
      ifelse(is.na(.) == T, 1, .)
  )


# Next, we'll do the same but for the "end" variable.

pollbase <-
  pollbase %>%
  mutate(
    end =
      end %>%
      na_if("?") %>%
      na_if("") %>%
      as.numeric() %>%
      ifelse(is.na(.) == T,
             paste(year, month, "01", sep = "-") %>%
               ymd() %>%
               ceiling_date("month") %>%
               subtract(1) %>%
               day(),
             .)
  )


# Now, we'll convert the start and end dates to date vectors and then drop
# the year and month variables. We'll also deal with dates in the original
# data which passed from one month to the next having the wrong month in
# up until now.

pollbase <-
  pollbase %>%
  mutate(
    start =
      paste0(year, month, start, sep = "-") %>%
      ymd(),
    end =
      paste0(year, month, end, sep = "-") %>%
      ymd()
  ) %>%
  mutate(
    end =
      ifelse(
        end < start,
        end %m+% months(1),
        end
      ) %>%
      as_date()
  ) %>%
  select(
    -year,
    -month
  )


# Next we need to convert the party variables to numeric proportions

pollbase <-
  pollbase %>%
  mutate(
    con = as.numeric(con)/100,
    lab = as.numeric(lab)/100,
    lib = as.numeric(lib)/100
  )


# Now, we'll filter out all PollBase data before the 2010 election and
# add in an empty column to hold the sample size

pollbase <-
  pollbase %>%
  filter(end < "2010-05-06") %>%
  mutate(n = NA) %>%
  relocate(n, .before = "con")



# 3. Load post-2010 data and transform ------------------------------------

# We'll now merge in the subsequent data that we pull from Wikipedia

pollbase <-
  rbind(
    pollbase,
    get_2015_polls(),
    get_2017_polls(),
    get_2019_polls(),
    get_new_polls()
  )


# Now, we'll give each poll a unique ID

pollbase <-
  pollbase %>%
  mutate(id = paste0("poll-", row_number()))


# Next, we'll assign each poll to its respective prior election

pollbase <-
  pollbase %>%
  mutate(election = get_last_election(start))


# Then, we'll select only those variables that we want to carry over to
# the modelling stage.

pollbase <-
  pollbase %>%
  select(
    id,
    election,
    start,
    end,
    pollster,
    n,
    con,
    lab,
    lib
  )


# We'll also add an indicator to show which party controlled the government
# at each election

pollbase <-
  pollbase %>%
  mutate(
    govt =
      case_when(
        election == as_date("1935-11-14") ~ 3,
        election == as_date("1945-07-05") ~ 2,
        election == as_date("1950-02-23") ~ 2,
        election == as_date("1951-10-25") ~ 2,
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
      labelled(labels = c(Conservative = 1, Labour = 2, National = 3))
  ) %>%
  relocate(govt, .before = "start")


# Plus we'll also add indicators that show who led each party on each start date

pollbase <-
  pollbase %>%
  mutate(
    con_ldr =
      case_when(
        start < as_date("1955-04-21") ~ 0,
        start >= as_date("1955-04-21") & start < as_date("1957-01-22") ~ 1,
        start >= as_date("1957-01-22") & start < as_date("1963-11-11") ~ 2,
        start >= as_date("1963-11-11") & start < as_date("1965-07-27") ~ 3,
        start >= as_date("1965-07-27") & start < as_date("1975-02-11") ~ 4,
        start >= as_date("1975-02-11") & start < as_date("1990-11-27") ~ 5,
        start >= as_date("1990-11-27") & start < as_date("1997-06-19") ~ 6,
        start >= as_date("1997-06-19") & start < as_date("2001-09-13") ~ 7,
        start >= as_date("2001-09-13") & start < as_date("2003-11-06") ~ 8,
        start >= as_date("2003-11-06") & start < as_date("2005-12-06") ~ 9,
        start >= as_date("2005-12-06") & start < as_date("2016-07-11") ~ 10,
        start >= as_date("2016-07-11") & start < as_date("2019-07-23") ~ 11,
        start >= as_date("2019-07-23") ~ 12
      ) %>%
      labelled(
        labels =
          c(
            `Winston Churchill` = 0,
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
        start >= as_date("1935-10-08") & start < as_date("1955-12-07") ~ 1,
        start >= as_date("1955-12-07") & start < as_date("1955-12-14") ~ 2,
        start >= as_date("1955-12-14") & start < as_date("1963-01-18") ~ 3,
        start >= as_date("1963-01-18") & start < as_date("1963-02-14") ~ 4,
        start >= as_date("1963-02-14") & start < as_date("1976-04-05") ~ 5,
        start >= as_date("1976-04-05") & start < as_date("1980-11-10") ~ 6,
        start >= as_date("1980-11-10") & start < as_date("1983-10-02") ~ 7,
        start >= as_date("1983-10-02") & start < as_date("1992-07-18") ~ 8,
        start >= as_date("1992-07-18") & start < as_date("1994-05-12") ~ 9,
        start >= as_date("1994-05-12") & start < as_date("1994-07-21") ~ 10,
        start >= as_date("1994-07-21") & start < as_date("2007-06-24") ~ 11,
        start >= as_date("2007-06-24") & start < as_date("2010-05-11") ~ 12,
        start >= as_date("2010-05-11") & start < as_date("2010-09-25") ~ 13,
        start >= as_date("2010-09-25") & start < as_date("2015-05-08") ~ 14,
        start >= as_date("2015-05-08") & start < as_date("2015-09-12") ~ 13,
        start >= as_date("2015-09-12") & start < as_date("2020-04-04") ~ 15,
        start >= as_date("2020-04-04") ~ 16
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
        start < as_date("1945-08-02") ~ 0,
        start >= as_date("1945-08-02") & start < as_date("1956-11-05") ~ 1,
        start >= as_date("1956-11-05") & start < as_date("1967-01-18") ~ 2,
        start >= as_date("1967-01-18") & start < as_date("1976-05-12") ~ 3,
        start >= as_date("1976-05-12") & start < as_date("1976-07-07") ~ 2,
        start >= as_date("1976-07-07") & start < as_date("1988-03-03") ~ 4,
        start >= as_date("1988-03-03") & start < as_date("1988-07-16") ~ 5,
        start >= as_date("1988-07-16") & start < as_date("1999-08-09") ~ 6,
        start >= as_date("1999-08-09") & start < as_date("2006-01-07") ~ 7,
        start >= as_date("2006-01-07") & start < as_date("2007-10-15") ~ 8,
        start >= as_date("2007-10-15") & start < as_date("2007-12-18") ~ 9,
        start >= as_date("2007-12-18") & start < as_date("2015-07-16") ~ 10,
        start >= as_date("2015-07-16") & start < as_date("2017-07-20") ~ 11,
        start >= as_date("2017-07-20") & start < as_date("2019-07-22") ~ 9,
        start >= as_date("2019-07-22") & start < as_date("2019-12-13") ~ 12,
        start >= as_date("2019-12-13") & start < as_date("2020-01-01") ~ 13,
        start >= as_date("2020-01-01") & start < as_date("2020-08-27") ~ 14,
        start >= as_date("2020-08-27") ~ 15
      ) %>%
      labelled(
        labels =
          c(
            `Archibald Sinclair` = 0,
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


# Finally, we'll add variable labels to inform users

var_label(pollbase) <-
  list(
    id = "Unique poll identification number",
    election = "Date of last general election",
    govt = "Largest party in government after the last general election",
    start = "First day of fieldwork",
    end = "Last day of fieldwork",
    pollster = "Polling company that conducted the poll",
    n = "Sample size",
    con = "Voting intention: Conservative",
    lab = "Voting intention: Labour",
    lib = "Voting intention: Liberal",
    con_ldr = "Leader of the Conservative Party",
    lab_ldr = "Leader of the Labour Party",
    lib_ldr = "Leader of the Liberals (various forms)"
  )



# 3. Save data ------------------------------------------------------------

# Now that we've processed the data, we can save it to "data"the disk. This is
# useful as it makes it much quicker to use the data and allows us to do
# so in other applications too.

usethis::use_data(
  pollbase,
  internal = FALSE,
  overwrite = TRUE
)


# Finally, we'll install and restart the package so that subsequent scripts
# call the most recent data.

devtools::install(upgrade = "never")



# 4. Produce replication data ---------------------------------------------

# Save system data to the "sessions" folder for the sake of transparency and
# future replication.

save_info(path = here("sessions", "002_pollbase.txt"))



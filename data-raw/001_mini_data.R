
# Create mini datasets

# 1. Housekeeping ---------------------------------------------------------

# Load packages

library(britpol)
library(tidyverse)
library(lubridate)
library(labelled)
library(hansard)
library(htmltab)
library(haven)
library(here)



# 2. Get election dates ---------------------------------------------------

# First, we'll get the data from Wikipedia

election_dates <-
  rbind(
    htmltab(
      doc = "https://w.wiki/36xA",
      which = 2
    ) %>%
      select(date = 2),
    htmltab(
      doc = "https://w.wiki/36xA",
      which = 3
    ) %>%
      select(date = 2)
  )


# Next, we'll sort out the dates and remove all cases before universal
# suffrage was introduced in 1928

election_dates <-
  election_dates %>%
  filter(
    str_detect(date, "The election") == F
  ) %>%
  mutate(
    date =
      date %>%
      str_remove(".*–") %>%
      trimws() %>%
      dmy()
  ) %>%
  na.omit() %>%
  filter(date > "1928-01-01") %>%
  arrange(date) %>%
  tibble() %>%
  distinct()


# Next, we'll give the data some variable labels

var_label(election_dates) <-
  list(date = "Date of election")


# Finally, we'll save the data to use later

usethis::use_data(
  election_dates,
  internal = FALSE,
  overwrite = TRUE
)



# 3. Create list of prime ministers ---------------------------------------

# Now, we'll get the list of British prime minister from Wikipedia then do
# some light editing.

prime_ministers <-
  htmltab(
    doc = "https://w.wiki/36wN",
    which = 2,
    header = 1:2
  ) %>%
  select(
    prime_minister = 1,
    pm_party = 6,
    start = 2,
    end = 3
  ) %>%
  tibble() %>%
  distinct()


# These data are terribly formatted, so we'll start by dealing with the
# dates so that we can filter out some of the crap.

prime_ministers <-
  prime_ministers %>%
  filter(
    !str_detect(start, "year|years|day|days"),
    !str_detect(start, "See also"),
    str_detect(start, "[:digit:]")
  ) %>%
  mutate(
    start = dmy(start),
    end =
      ifelse(
        end == "Incumbent",
        format(
          Sys.Date(),
          format = "%d %b%Y"
        ),
        end
      ) %>%
      dmy()
  )


# Now we can get rid of the crap in the prime_minister column

prime_ministers <-
  prime_ministers %>%
  mutate(
    prime_minister =
      prime_minister %>%
      str_remove("\\s*\\([^\\)]+\\)") %>%
      str_remove("MP for.*") %>%
      str_remove("Sir ") %>%
      str_remove("[:digit:].*")
  )


# And we'll also get rid of any brackets in the party column and
# convert it into a labelled variable

prime_ministers <-
  prime_ministers %>%
  mutate(
    pm_party =
      pm_party %>%
      str_remove("\\s*\\([^\\)]+\\)")
  )


# Next, we'll give the data some variable labels

var_label(prime_ministers) <-
  list(
    prime_minister = "Name of Prime Minister",
    pm_party = "Prime Minister's party",
    start = "Date of first day of Prime Minister's term",
    end = "Date of last day of Prime Minister's term"
  )


# Finally, we'll save the data to use later

usethis::use_data(
  prime_ministers,
  internal = FALSE,
  overwrite = TRUE
)



# 4. Create list of party leaders -----------------------------------------

# We'll again scrape Wikipedia for the data we need. Let's start with the
# Labour Party.

lab_ldr <-
  htmltab(
    doc = "https://w.wiki/36xE",
    which = 2
  ) %>%
  select(
    leader = 2,
    start = 4,
    end = 5
  ) %>%
  mutate(
    leader =
      leader %>%
      str_remove_all("\\s*\\([^\\)]+\\)") %>%
      str_remove("MP for.*") %>%
      str_remove("Sir "),
    start =
      start %>%
      str_remove_all("\\s*\\([^\\)]+\\)") %>%
      dmy(),
    end =
      end %>%
      str_remove_all("\\s*\\([^\\)]+\\)") %>%
      ifelse(. == "Incumbent", format(Sys.Date(), "%d %b %Y"), .) %>%
      dmy(),
    party = "Labour Party"
  ) %>%
  distinct()


# Next, we'll do the same for the Conservatives. We have to do this twice
# because they're such an old party. Sorry this code is gross.

con_ldr <-
  rbind(
    htmltab(
      doc = "https://w.wiki/36xL",
      which = 2
    ) %>%
      select(
        leader = 1,
        start = 3,
        end = 4
      ),
    htmltab(
      doc = "https://w.wiki/36xL",
      which = 3
    ) %>%
      select(
        leader = 1,
        start = 3,
        end = 4
      )
  ) %>%
  distinct() %>%
  mutate(
    leader =
      leader %>%
      str_remove_all("\\s*\\([^\\)]+\\)") %>%
      str_remove("MP for.*") %>%
      str_remove("Sir ") %>%
      str_remove(".*–") %>%
      trimws() %>%
      str_remove("[:digit:].*"),
    start =
      start %>%
      str_remove_all("\\s*\\([^\\)]+\\)") %>%
      dmy(),
    end =
      end %>%
      str_remove_all("\\s*\\([^\\)]+\\)") %>%
      str_remove(".* - ") %>%
      ifelse(. == "Incumbent", format(Sys.Date(), "%d %b %Y"), .) %>%
      dmy(),
    party = "Conservative Party"
  )


# Next, the liberals. Again, we have to do this twice.

lib_ldr <-
  rbind(
    htmltab(
      doc = "https://w.wiki/36xX",
      which = 1
    ) %>%
      select(
        leader = 1,
        start = 3,
        end = 4
      ) %>%
      filter(leader != "Leaders of the Liberal Party in the House of Commons") %>%
      mutate(
        leader =
          ifelse(
            str_detect(leader, "VACANT") == T,
            "Vacant",
            leader
          )
      ),
    htmltab(
      doc = "https://w.wiki/36xZ",
      which = 2
    ) %>%
      select(
        leader = 2,
        start = 4,
        end = 5
      )
  ) %>%
  distinct() %>%
  mutate(
    leader =
      leader %>%
      str_remove_all("\\s*\\([^\\)]+\\)") %>%
      str_remove("MP for.*") %>%
      str_remove("Sir ") %>%
      str_remove(".*–") %>%
      trimws() %>%
      str_remove(",.*") %>%
      str_remove("[:digit:].*") %>%
      str_remove("was|were.*") %>%
      str_remove("Acting.*") %>%
      trimws(),
    start =
      start %>%
      str_remove_all("\\s*\\([^\\)]+\\)") %>%
      dmy(),
    end =
      end %>%
      str_remove_all("\\s*\\([^\\)]+\\)") %>%
      str_remove(".* - ") %>%
      ifelse(. == "Incumbent", format(Sys.Date(), "%d %b %Y"), .) %>%
      dmy(),
    party = "Liberals (Various Forms)"
  ) %>%
  mutate(
    end =
      case_when(
        is.na(end) == T & is.na(lead(start, 1)) == F ~ lead(start, 1),
        is.na(end) == T & is.na(lead(start, 1)) == T ~ lead(end, 1),
        start == end ~ lead(start, 1),
        TRUE ~ end
      ),
    start =
      case_when(
        is.na(start) == T ~ lag(end, 1),
        TRUE ~ start
      )
  ) %>%
  distinct()

# Now we can combine them

party_leaders <-
  rbind(
    con_ldr,
    lab_ldr,
    lib_ldr
  ) %>%
  relocate("party", .before = "start")


# Next, we'll give the data some variable labels

var_label(party_leaders) <-
  list(
    leader = "Name of party leader",
    party = "Party leader's party",
    start = "Date of first day of leader's term",
    end = "Date of last day of leader's term"
  )


# Finally, we'll save the data to use later

usethis::use_data(
  party_leaders,
  internal = FALSE,
  overwrite = TRUE
)



# 5. Get historic constituency results ------------------------------------

# First, let's download the historic election results

election_results <- britpol:::load_results()


# Next, we'll filter out any pre-1928 cases

election_results <-
  election_results %>%
  filter(election >= 1928)


# And then we'll select the variables we need and convert to a tibble

election_results <-
  election_results %>%
  select(
    constituency,
    seats,
    region = country.region,
    election,
    electorate,
    con_votes,
    lab_votes,
    lib_votes,
    nat_votes = natSW_votes,
    oth_votes
  ) %>%
  tibble()


# Now, we'll convert the constituency names to title case

election_results <-
  election_results %>%
  mutate(
    constituency =
      constituency %>%
      snakecase::to_title_case()
  )


# And we'll replace the election year with the actual election date

election_results <-
  election_dates %>%
  mutate(
    year = ifelse(month(date) == 10 & year(date) == 1974, 1974.5, year(date)),
    month = month(date)
  ) %>%
  right_join(
    election_results %>%
      mutate(
        election =
          case_when(
            election == "1974F" ~ "1974",
            election == "1974O" ~ "1974.5",
            TRUE ~ election
          ) %>%
          as.numeric()
      ),
    by = c("year" = "election")
  ) %>%
  select(-year, -month)


# Next, we'll give the data some variable labels

var_label(election_results) <-
  list(
    constituency = "Name of parliamentary constituency",
    seats = "Number of seats the constituency contains",
    region = "Region or country",
    election = "Date of election",
    electorate = "Size of electorate",
    con_votes = "Number of Conservative votes",
    lab_votes = "Number of Labour votes",
    lib_votes = "Number of Liberal votes",
    nat_votes = "Number of Nationalist votes (Scotland and Wales only)",
    oth_votes = "Number of other votes"
  )


# Finally, we'll save the data to the package

usethis::use_data(
  election_results,
  internal = FALSE,
  overwrite = TRUE
)



# 6. Get historic constituencies ------------------------------------------

# First, let's download the constituency data using the hansard package

constituencies <- constituencies()


# Now, we'll select and rename the variables

constituencies <-
  constituencies %>%
  select(
    name = label_value,
    gss_code,
    start = started_date_value,
    end = ended_date_value
  )


# Next, we'll organise them by start date and name, convert the
# start and end dates from POSIXct to date format, and mark any
# empty gss_codes as NA.

constituencies <-
  constituencies %>%
  arrange(start, name) %>%
  mutate(
    gss_code = ifelse(gss_code == "", NA, gss_code),
    start = as_date(start),
    end = as_date(end)
  )


# The data contain a dummy constituency for some reason, so let's
# remove it

constituencies <-
  constituencies %>%
  filter(name != "Dummy constituency")


# Let's also remove any diacritics from the constituency names too

constituencies <-
  constituencies %>%
  mutate(
    name = iconv(name, from = "UTF-8", to = "ASCII//TRANSLIT")
  )


# Next, we'll give the data some variable labels

var_label(constituencies) <-
  list(
    name = "Name of parliamentary constituency",
    gss_code = "Government Statistical Service code",
    start = "Date constituency was introduced",
    end = "Date constituency was retired"
  )


# Finally, we'll save the data to the package

usethis::use_data(
  constituencies,
  internal = FALSE,
  overwrite = TRUE
)



# 7. Create list of red wall constituencies -------------------------------

# Create tibble

red_wall <-
  tibble(
    name =
      c(
        "Bury South",
        "Bolton North East",
        "Oldham East and Saddleworth",
        "Heywood and Middleton",
        "Chorley",
        "Hyndburn",
        "Burnley",
        "Blackpool South",
        "Wirral South",
        "Scunthorpe",
        "Great Grimsby",
        "Penistone and Stocksbridge",
        "Rother Valley",
        "Don Valley",
        "Halifax",
        "Batley and Spen",
        "Wakefield",
        "Bradford South",
        "Hemsworth",
        "North West Durham",
        "Darlington",
        "Sedgefield",
        "Bishop Auckland",
        "Tynemouth",
        "Newcastle upon Tyne North",
        "Newcastle-under-Lyme",
        "Stoke-on-Trent Central",
        "Stoke-on-Trent North",
        "Coventry South",
        "Coventry North West",
        "Birmingham, Northfield",
        "Wolverhampton North East",
        "West Bromwich West",
        "Dudley North",
        "Chesterfield",
        "Bolsover",
        "Gedling",
        "Bassetlaw",
        "Ashfield"
      ),
    gss_code =
      c(
        "E14000535",
        "E14000546",
        "E14000548",
        "E14000565",
        "E14000569",
        "E14000573",
        "E14000577",
        "E14000578",
        "E14000588",
        "E14000609",
        "E14000612",
        "E14000632",
        "E14000637",
        "E14000650",
        "E14000651",
        "E14000658",
        "E14000667",
        "E14000671",
        "E14000856",
        "E14000710",
        "E14000716",
        "E14000723",
        "E14000740",
        "E14000747",
        "E14000758",
        "E14000834",
        "E14000833",
        "E14000870",
        "E14000876",
        "E14000903",
        "E14000914",
        "E14000915",
        "E14000972",
        "E14000973",
        "E14001006",
        "E14001009",
        "E14001030",
        "E14001043",
        "E14001049"
      )
  )


# Clean constituency names

red_wall <-
  red_wall %>%
  mutate(
    name =
      name %>%
      clean_pcon_names()
  )


# Next, we'll give the data some variable labels

var_label(red_wall) <-
  list(
    name = "Name of parliamentary constituency",
    gss_code = "Government Statistical Service code"
  )


# Finally, we'll save the data to use later

usethis::use_data(
  red_wall,
  internal = FALSE,
  overwrite = TRUE
)



# 8. Create replication info ----------------------------------------------

# We'll install and restart the package so that subsequent scripts call the
# most recent data.

devtools::install(upgrade = "never")


# Now that we've saved all of our data, we can save the session information
# so that we can recall it later if needed.

britpol:::save_info(here("sessions", "001_mini_data.txt"))


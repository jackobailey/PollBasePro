
# Create mini datasets

# 1. Housekeeping ---------------------------------------------------------

# Load packages

library(tidyverse)
library(lubridate)
library(labelled)
library(janitor)
library(hansard)
library(htmltab)
library(readxl)
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

# Get index of first sheet post-1928

start <-
  here("inst", "extdata", "1918-2019election_results_by_pcon.xlsx") %>%
  excel_sheets() %>%
  str_detect("1929") %>%
  which()


# Get number of sheets

sheets <-
  here("inst", "extdata", "1918-2019election_results_by_pcon.xlsx") %>%
  excel_sheets() %>%
  length()


# Load raw data from Mark Pack's website

constituency_results <-
  tibble(
    date = election_dates$date,
    n = start:sheets
  ) %>%
  mutate(
    data =
      map2(
        .x = n,
        .y = date,
        .f = function(x, y){

          # Read data

          dta <-
            read_xlsx(
              here("inst", "extdata", "1918-2019election_results_by_pcon.xlsx"),
              sheet = x,
              col_types = "text",
              .name_repair = "minimal",
              skip = 2
            )


          # Rename headers and drop first row

          names(dta) <-
            paste0(dta[1, ], names(dta)) %>%
            str_remove("^Votes") %>%
            tolower() %>%
            str_remove("^na") %>%
            str_remove("^ons ")
          dta <- dta[-1, ]


          # Clean data set

          dta <-
            dta %>%
            drop_na(id) %>%
            remove_constant() %>%
            select(
              -any_of(""),
              -matches("vote share"),
              -`total votes`,
              -turnout
            ) %>%
            rename(region = `country/region`) %>%
            mutate(
              constituency =
                constituency %>%
                stringr::str_replace(",", ", ") %>%
                stringr::str_replace("\\)", "\\) ") %>%
                stringr::str_replace("&", "and") %>%
                tolower %>%
                stringr::str_replace("st\\.", "st\\ ") %>%
                iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
                tools::toTitleCase() %>%
                stringr::str_replace("  ", " ")
            )


          # Create seats variable if none exists

          if(sum(str_detect("seats", names(dta))) == 0){
            dta <-
              dta %>%
              mutate(seats = 1) %>%
              relocate(seats, .after = "id")
          }


          # Pivot to long format

          dta <-
            dta %>%
            pivot_longer(
              cols = c(-id, -seats, -constituency, -county, -region, -electorate),
              names_to = "party",
              values_to = "votes"
            )


          # Add date

          dta <-
            dta %>%
            mutate(date = y) %>%
            relocate(date, .before = "id")


          # Return to user

          return(dta)

        }
      )
  )


# Convert to tibble and recode

constituency_results <-
  do.call("rbind.data.frame", constituency_results$data) %>%
  mutate(
    party =
      party %>%
      britpol::clean_party_names(
        party_names =
          list(
            `^con|^tor` = "con",
            `^lab` = "lab",
            `^lib|^ld|^alliance$` = "lib",
            `^snp|^scotnat|^scottishnat|^pc|^plaid|wales` = "nat"
          )
      ) %>%
      str_replace("Other", "oth"),
    votes = as.numeric(votes)
  ) %>%
  group_by(date, id, seats, constituency, county, region, electorate, party) %>%
  summarise(
    votes = sum(votes, na.rm = T),
    .groups = "drop")


# Now we'll pivot the data to wide format

constituency_results <-
  constituency_results %>%
  mutate(votes = ifelse(votes == 0, NA, votes)) %>%
  pivot_wider(
    names_from = "party",
    values_from = "votes"
  )


# Sort out id number

constituency_results <-
  constituency_results %>%
  mutate(
    scheme =
      case_when(
        year(date) < 1955 ~ NA_character_,
        year(date) > 1955 & year(date) <= 1979 ~ "pano",
        year(date) > 1979 & year(date) <= 1992 ~ "nomis",
        year(date) > 1992 & year(date) <= 2001 ~ "pca",
        year(date) > 2001 & year(date) < 2010 & region != "Scotland" ~ "pca",
        year(date) > 2001 & year(date) < 2010 & region == "Scotland" ~ "ons",
        year(date) >= 2010 ~ "ons"
      ),
    id = ifelse(is.na(scheme) == T, NA, id)
  ) %>%
  pivot_wider(
    names_from = "scheme",
    values_from = "id"
  ) %>%
  remove_constant() %>%
  relocate(c(pano, nomis, pca, ons), .before = "county")


# Convert vote and seat counts to numeric

constituency_results <-
  constituency_results %>%
  mutate(
    seats = as.numeric(seats),
    electorate = as.numeric(electorate),
    con = as.numeric(con),
    lab = as.numeric(lab),
    lib = as.numeric(lib),
    oth = as.numeric(oth),
    nat = as.numeric(nat)
  ) %>%
  relocate("nat", .before = "oth")


# Merge in post-2010 PANO data thanks to Chris Hanretty

load(here("inst", "extdata", "pano_data.rda"))

constituency_results <-
  left_join(
    constituency_results,
    pano_data,
    by = c("ons" = "ons_id")
  ) %>%
  mutate(
    pa_id = as.character(pa_id),
    pano =
      case_when(
        year(date) >= 2010 ~ pa_id,
        TRUE ~ pano
      )
  ) %>%
  select(
    -parlparse_id,
    -hansard_id,
    -name,
    -pa_id,
    -regex
  )


# Merge in 92-05 PANO data thanks to Justin Fisher

pano_92_05 <-
  read_csv(here("inst", "extdata", "pano92_05.csv")) %>%
  mutate(
    name =
      name %>%
      britpol::clean_pcon_names()
  ) %>%
  rename(pano92 = pano)

constituency_results <-
  constituency_results %>%
  mutate(year = year(date)) %>%
  left_join(
    pano_92_05,
    by = c("constituency" = "name", "year")
  ) %>%
  mutate(
    pano = ifelse(is.na(pano) == T, pano92, pano)
  ) %>%
  select(
    -year,
    -pano92
  )


# Add missing counties


# Next, we'll give the data some variable labels

var_label(constituency_results) <-
  list(
    date = "Date of election",
    seats = "Number of seats the constituency contains",
    constituency = "Name of parliamentary constituency",
    pano = "Press Association code",
    nomis = "NOMIS code",
    pca = "PCA code",
    ons = "ONS GSS code",
    county = "County",
    region = "Region or country",
    electorate = "Size of electorate",
    con = "Number of Conservative votes",
    lab = "Number of Labour votes",
    lib = "Number of Liberal votes",
    nat = "Number of Nationalist votes (Scotland and Wales only)",
    oth = "Number of other votes"
  )


# Finally, we'll save the data to the package

usethis::use_data(
  constituency_results,
  internal = FALSE,
  overwrite = TRUE
)



# 6. Create list of red wall constituencies -------------------------------

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
      britpol::clean_pcon_names()
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



# 7. Create replication info ----------------------------------------------

# Now that we've saved all of our data, we can save the session information
# so that we can recall it later if needed.

britpol:::save_info(here("sessions", "001_mini_data.txt"))


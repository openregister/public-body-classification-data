# Update the list with the data from 2017-10-31,
# in particular combining all housing associations into a single 'en bloc'
# record.

library(tidyverse)
library(readxl)
library(stringr)
library(here)

map_path <- here("lists", "name-to-curie-map.tsv")
old_register_path <- here("data", "public-body.tsv")
old_list_path <- here("lists", "list.xls")
new_list_path <- here("lists", "list-2017-10-31.xls")
new_records_path <- here("lists", "new-records-2017-10-31.tsv")
public_body_path <- here("data", "public-body.tsv")

old_map <- read_tsv(map_path)
old_register <- read_tsv(old_register_path)

old_list <-
  read_excel(old_list_path, sheet = 8, range = "A5:D4854") %>%
    select(Institutions,
           Sector,
           `ESA10 category`,
           `Classification applies from`) %>%
    rename(name = Institutions,
           sector = Sector,
           `esa-2010` = `ESA10 category`,
           `start-date` = `Classification applies from`)

new_list <-
  read_excel(new_list_path, sheet = 8, range = "A5:D3025") %>%
    select(Institutions,
           Sector,
           `ESA10 category`,
           `Classification applies from`) %>%
    rename(name = Institutions,
           sector = Sector,
           `esa-2010` = `ESA10 category`,
           `start-date` = `Classification applies from`)

# Ones whose dates must be set manually
# * Government Legal Department	Central Government	S.1311	27-Jun-1876
# * Local Government Pension Scheme	Public Pension Funds	S.12901	Since the start of the scheme
# * NHS Charities in England and Wales (En Bloc)	Central Government	S.1311	Various
# * Regional Aggregation Bodies	Former Central Government	S.1311	Jun-06 - Aug-10
# * Regional Assemblies (England)	Former Local Government	S.1313	2008-2010
# * Studio Schools (en Bloc)	Central Government	S.1311	Various
# * University Technical Colleges (en Bloc)	Central Government	S.1311	Various

still_exist <- inner_join(old_list, new_list, by = "name")
removed <- anti_join(old_list, new_list, by = "name")
added <- anti_join(new_list, old_list, by = "name")

same_classification <- filter(still_exist, `esa-2010.x` == `esa-2010.y`)
new_classification <- filter(still_exist, `esa-2010.x` != `esa-2010.y`) # spurious

na_start_date <- filter(still_exist, is.na(`start-date.x`) & is.na(`start-date.y`))
same_start_date <- filter(still_exist, `start-date.x` == `start-date.y`)
new_start_date <- filter(still_exist, `start-date.x` != `start-date.y`)

# new records to add
new_records <-
  added %>%
  rename(`public-body-classifications` = `esa-2010`) %>%
  mutate(`end-date` = if_else(str_sub(sector, 1, 6) == "Former",
                               `start-date`,
                                as.POSIXct(NA_real_, origin = "1970-01-01"))) %>%
  mutate(`start-date` = if_else(str_sub(sector, 1, 6) == "Former",
                                as.POSIXct(NA_real_, origin = "1970-01-01"),
                                `start-date`)) %>%
  mutate(`public-body` = c("1737", seq_len(n() - 1) + max(old_register$`public-body`)),
         organisations = c("government-organisation:OT360", rep(NA, n() - 1)),
         `start-date` = format(`start-date`, "%Y-%m-%d"),
         `end-date` = format(`end-date`, "%Y-%m-%d")) %>%
  select(`public-body`, name, organisations, `public-body-classifications`, `start-date`, `end-date`) %>%
  bind_rows(tibble(`public-body` = "4613",
                   name = "Forth Estuary Transport Authority",
                   organisations = NA,
                   `public-body-classification` = "S.1311",
                   `start-date` = "2008-02-01",
                   `end-date` = "2015-04-30"))

id_of_duplicates <-
  filter(old_map, name %in% c("Developing Initiatives for Support in the Community", "North Devon Homes Limited"), is.na(`start-date`)) %>%
  pull(`public-body`)

id_to_remove <- c(4613, 1737)
# 4613	Forth Estuary Transport Authority
# 1737	Green Investment Bank

removed_by_name <- inner_join(old_map, removed, by = "name")
removed_by_curie <- anti_join(removed, removed_by_name, by = "name")

new_map <-
  new_list %>%
  left_join(distinct(old_map, name, `public-body`, organisation)) %>%
  filter(!is.na(`public-body`),
         !(`public-body` %in% id_of_duplicates),
         !(`public-body` %in% id_to_remove)) %>%
  anti_join(removed_by_name, by = "public-body") %>%
  mutate(`end-date` = if_else(str_sub(sector, 1, 6) == "Former",
                               `start-date`,
                                as.POSIXct(NA_real_, origin = "1970-01-01"))) %>%
  mutate(`start-date` = if_else(str_sub(sector, 1, 6) == "Former",
                                as.POSIXct(NA_real_, origin = "1970-01-01"),
                                `start-date`)) %>%
  mutate(`public-body` = as.character(`public-body`),
         `start-date` = format(`start-date`, "%Y-%m-%d"),
         `end-date` = format(`end-date`, "%Y-%m-%d")) %>%
  bind_rows(new_records)

new_register <-
  new_map %>%
  mutate(name = ifelse(!is.na(organisations), NA_character_, name)) %>%
  arrange(desc(name), desc(organisations)) %>%
  write_tsv(public_body_path, na = "")

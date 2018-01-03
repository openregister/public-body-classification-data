# Update the list with the data from 2017-12-29,
# in particular combining all housing associations into a single 'en bloc'
# record.

library(tidyverse)
library(readxl)
library(stringr)
library(RegistersClientR)
library(lubridate)
library(here)

map_path <- here("lists", "name-to-curie-map.tsv")
old_list_path <- here("lists", "list-2017-11-30.xls")
new_list_path <- here("lists", "list-2017-12-29.xls")
new_records_path <- here("lists", "new-records-2017-12-29.tsv")
public_body_path <- here("data", "public-body.tsv")

old_map <- read_tsv(map_path)
old_register <-
  rr_records("public-body-account", "alpha") %>%
  select(`public-body-account`, name, organisation, `public-body-account-classifications`, `start-date`, `end-date`)

old_list <-
  read_excel(old_list_path, sheet = 8, range = "A5:D3025") %>%
    select(Institutions,
           Sector,
           `ESA10 category`,
           `Classification applies from`) %>%
    rename(name = Institutions,
           sector = Sector,
           `esa-2010` = `ESA10 category`,
           `start-date` = `Classification applies from`)

new_list <-
  read_excel(new_list_path, sheet = 8, range = "A5:D3026") %>%
    select(Institutions,
           Sector,
           `ESA10 category`,
           `Classification applies from`) %>%
    rename(name = Institutions,
           sector = Sector,
           `esa-2010` = `ESA10 category`,
           `start-date` = `Classification applies from`)

# Compare old list with new list

still_exist <- inner_join(old_list, new_list, by = "name")
removed <- anti_join(old_list, new_list, by = "name")
added <- anti_join(new_list, old_list, by = "name")

new_classification <- filter(still_exist, `esa-2010.x` != `esa-2010.y`)
new_sector <- filter(still_exist, `sector.x` != `sector.y`)

different_start_date <- filter(still_exist, `start-date.x` != `start-date.y`)
new_start_date <- filter(still_exist, is.na(`start-date.x`), !is.na(`start-date.y`))
removed_start_date <- filter(still_exist, !is.na(`start-date.x`), is.na(`start-date.y`))

old_list %>% filter(str_detect(name, "Private registered"))

# Look at the changes
removed
added
new_classification
new_sector
different_start_date
new_start_date
removed_start_date

# This month's changes (comparing old list with new list)

# "Private registered providers (PRPs) of social housing in England Public Non-Financial Corporation"
# 5860
# Name changed to: "Private registered providers' of social housing (including most housing associations) in England"
# Reclassified as not-public-body on 2017-11-16 (to be end-dated)

# "Crown Estate Scotland (Interim Management)"
# Added on 2017-04-01 (to be start-dated)
# Classified as S.11001

# "Food from Britain"
# 1239
# Reclassified from S.1.311 to S.1311 (old version was a typo that was missed)
# Reclassified from "Central Government" to "Former Central Government" on 2014-07-18 (to be end-dated)

# "Commission for Rural Communities"
# 1117
# Reclassified from "Central Government" to "Former Central Government" on 2013-03-31 (to be end-dated)

# "Committee on Agricultural Valuation"
# 4146
# Reclassified from "Central Government" to "Former Central Government" on 2014-04-23 (to be end-dated)

# Complete the missing maps in the old register, e.g. Aberdeen City Council
# should map to a government organisation
old_register_with_all_maps <-
  old_register %>%
  left_join(select(old_map, `public-body-account`, name, organisation), by = "public-body-account") %>%
  mutate(organisation = if_else(is.na(organisation.x), organisation.y, organisation.x)) %>%
  select(-organisation.x, -organisation.y, -name.y) %>%
  rename(name = name.x) %>%
  mutate(name = if_else(is.na(organisation), name, NA_character_))
write_tsv(old_register_with_all_maps, here("data", "new-register-with-all-maps.tsv"))

# Records in the old register that aren't in the map
old_register_unmapped <-
  old_register_with_all_maps %>%
  left_join(select(old_map, `public-body-account`, name), by = "public-body-account") %>%
  filter(is.na(name.y))

# Compare old register with new list
old_register_with_all_names <-
  old_register_with_all_maps %>%
  left_join(select(old_map, `public-body-account`, name), by = "public-body-account") %>%
  select(`public-body-account`, name = name.y, `esa-2010` = `public-body-account-classifications`, `start-date`, `end-date`)

still_exist <- inner_join(old_register_with_all_names, new_list, by = "name")
removed <- anti_join(old_register_with_all_names, new_list, by = "name")
added <- anti_join(new_list, old_register_with_all_names, by = "name")

new_classification <- filter(still_exist, `esa-2010.x` != `esa-2010.y`)

# new sector and dates can't be detected here, because there are so many records
# that were already supposed to be end-dated when we got the first list, but the
# list doesn't tell us the date they should have been end-dated.

# Look at the changes
removed
added
new_classification

# All the 'removed' records actually just need to be added to the map file.
# "Crown Estate (Scotland)" has already been covered by the old/new-list comparison.
# "Green Investment Bank" actually just needs to be added to the map file (it's already there under a different name)
# "Food From Britain" has already been covered by the old/new-list comparison.

max(old_register$`public-body-account`)

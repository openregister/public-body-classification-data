# A script to follow each month to update the register with the latest data

library(tidyverse)
library(readxl)
library(stringr)
library(lubridate)
library(RegistersClientR)
library(here)

new_list_path <- here("lists/list-2017-11-30.xls")
curie_map_path <- here("lists/map.tsv")

curie_map <- read_tsv(curie_map_path)
new_list <-
  read_excel(new_list_path, sheet = 8, skip = 4) %>%
    select(Institutions,
           Sector,
           `ESA10 category`,
           `Classification applies from`) %>%
    rename(name = Institutions,
           sector = Sector,
           `esa-2010` = `ESA10 category`,
           `date` = `Classification applies from`)

register <-
  rr_records("public-body-account", "alpha") %>%
  select(`public-body-account`, name, organisation, `public-body-account-classifications`, `start-date`, `end-date`)

# Each organisation (curie) should only appear once
max(table(register$organisation)) == 1

# Each record should have either a name or an organisation but not both
tally(filter(register, is.na(name), is.na(organisation)))$n == 0
tally(filter(register, !is.na(name), !is.na(organisation)))$n == 0

# Each name in the map should appear exactly once (but there might be more than
# one name per `public-body-account`)
max(table(curie_map$name)) == 1

# Each record should appear in the map
length(setdiff(register$`public-body-account`,
               curie_map$`public-body-account`)) == 0

# Find entries in the list that
# * are in the map
# * aren't in the map
in_map <-
  inner_join(new_list, curie_map, by = "name") %>%
  select(`public-body-account`, name, organisation, everything())
not_in_map <- anti_join(new_list, curie_map, by = "name")

# Of the entries that aren't in the map
# * Check that they aren't just a respelling of something in the map (manual)
# * If it's just a respelling, add the new spelling to the map
# * If it's genuinely new (or is a new respelling), add it to the map (choosing
# a new value for `public-body-account` that hasn't been used before, mapping
# the esa-2010 code to one in the `public-body-account-classification` register,
# and choosing the start-date and end-date.  If the sector is "Former ..." then
# the `date` becomes the `end-date`, otherwise it becomes the `start-date`.

# All the entries should now be in the map.
in_map <-
  inner_join(new_list, curie_map, by = "name") %>%
  select(`public-body-account`, name, organisation, everything())

# Compare the new list, mapped to `public-body-account` IDs, with the register.
# Find any that have changed.
new_name <-
  inner_join(in_map, register, by = "public-body-account") %>%
  filter(name.x != name.y) %>%
  select(`public-body-account`, name.x, name.y)
new_organisation <-
  inner_join(in_map, register, by = "public-body-account") %>%
  filter(organisation.x != organisation.y) %>%
  select(`public-body-account`, organisation.x, organisation.y)
new_classification <-
  inner_join(in_map, register, by = "public-body-account") %>%
  filter(`esa-2010` != `public-body-account-classifications`) %>%
  select(`public-body-account`, `esa-2010`, `public-body-account-classifications`)
new_start_date <-
  inner_join(in_map, register, by = "public-body-account") %>%
  filter(!str_detect(sector, "^[fF]ormer"),
         date != `start-date`) %>%
  select(`public-body-account`, sector, date, `start-date`, `end-date`)
new_end_date <-
  inner_join(in_map, register, by = "public-body-account") %>%
  filter(str_detect(sector, "^[fF]ormer"),
         date != `end-date`) %>%
  select(`public-body-account`, sector, date, `start-date`, `end-date`)

new_classification
new_start_date
new_end_date

# Of the entries in the list that are in the map and have changed
# * If their sector has changed from "Something" to "Former Something", then the
# `date` should become the `end-date`, otherwise the date should become the
# `start-date`
# * If the sector has changed, find the appropriate ID for it from the
# `public-body-account-classification` register.
# * Update the register with the new and amended records

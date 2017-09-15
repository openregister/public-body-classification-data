library(tidyverse)
library(stringr)
library(here)

data_path <- here("data", "public-body.tsv")

original <-
  read_csv(file.path(here(), "lists", "public-body.csv")) %>%
  select(name, sector, `start-date`)
orgs <-
  read_csv(file.path(here(), "lists", "organisation.csv")) %>%
  select(-`start-date`, -`end-date`)

jamie <-
  read_csv(file.path(here(), "lists", "manually-joined-jamie.csv")) %>%
  distinct() # duplicates somehow crept in at the auto-join stage
duncan <-
  read_csv(file.path(here(), "lists", "manually-joined-duncan.csv")) %>%
  distinct() # duplicates somehow crept in at the auto-join stage

# The first 1600 were auto-matched.  Jamie checked them.  The matching script
# only updates the name, not the curie, so look it up again in the orgs file.
# Where the name is NA, it matches to two, so filter those out for Jamie to fix.
jamie1600 <-
  jamie %>%
  slice(1:1600) %>%
  rename(ons_name = name, org_name = name_1) %>%
  select(-organisation) %>%
  left_join(orgs, by = c("org_name" = "name"))

# These are the ones that Jamie found to have multiple matches, so need manually
# editing.
jamie_multiples <-
  jamie1600 %>%
  filter(is.na(org_name)) %>%
  select(-org_name)

jamie_others <-
  jamie %>%
  slice(c(2501:3500, 4501:5559)) %>%
  rename(ons_name = name, org_name = name_1) %>%
  select(-organisation) %>%
  left_join(orgs, by = c("org_name" = "name")) %>%
  select(-org_name)

duncan_all <-
  duncan %>%
  slice(c(1601:2500, 3501:4500)) %>%
  rename(ons_name = name, org_name = name_1) %>%
  select(-organisation) %>%
  left_join(orgs, by = c("org_name" = "name")) %>%
  select(-org_name)

combined <-
  bind_rows(jamie1600, jamie_others, duncan_all) %>%
  left_join(original, by = c("ons_name" = "name")) %>%
  mutate(organisation = if_else(ons_name == "Internal Drainage Boards",
                                "register:internal-drainage-board",
                                organisation)) %>%
  mutate(`public-body` = row_number() + 1000) %>%
  rename(name = ons_name,
         `public-body-classification` = `esa-2010`) %>%
  select(`public-body`, name, organisation, `public-body-classification`, sector, `start-date`)

# Should have 4848 rows altogether
nrow(combined) == 4848

# when a public body is classified 'Former X', then the 'Classification applies
# from' date should be used as the end-date, rather than the start-date, which
# should be NA.
dated <-
  combined %>%
  mutate(`end-date` = if_else(str_sub(sector, 1, 6) == "Former",
                               `start-date`,
                                as.POSIXct(NA_real_, origin = "1970-01-01"))) %>%
  mutate(`start-date` = if_else(str_sub(sector, 1, 6) == "Former",
                                as.POSIXct(NA_real_, origin = "1970-01-01"),
                                `start-date`)) %>%
  select(-sector)

dated %>%
  filter(str_detect(tolower(name), "en bloc")) %>%
  print(n = Inf)

dated %>%
  mutate(register = str_extract(organisation, ".*:")) %>%
  distinct(register)

write_tsv(dated, data_path)

# Write NA as blank
# Write dates with resolution of one day
# Remove names when the organisation field is already populated

library(tidyverse)
library(here)

public_body_path <- here("data", "public-body.tsv")

read_tsv(public_body_path) %>%
  mutate(name = ifelse(!is.na(organisations), NA_character_, name),
         `start-date` = format(`start-date`, "%Y-%m-%d"),
         `end-date` = format(`end-date`, "%Y-%m-%d")) %>%
  write_tsv(public_body_path, na = "")


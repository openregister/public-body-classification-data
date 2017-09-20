# Write NA as blank
# Remove names when the organisation field is already populated

library(tidyverse)
library(here)

public_body_path <- here("data", "public-body.tsv")

read_tsv(public_body_path) %>%
  mutate(name = ifelse(!is.na(organisations), NA_character_, name)) %>%
  write_tsv(public_body_path, na = "")


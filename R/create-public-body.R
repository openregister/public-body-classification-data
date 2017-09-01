# From lists/auto-joined.csv, create a first draft of data/public-body.tsv

library(tidyverse)
library(stringr)
library(here)

source_file <- file.path(here(), "lists", "auto-joined.csv")
dest_file <- file.path(here(), "data", "public-body.tsv")

source_file %>%
  read_csv() %>%
  mutate(`public-body` = row_number()) %>%
  rename(classes = `esa-2010`) %>%
  filter(!is.na(classes)) %>%
  select(`public-body`, name, organisation, classes) %>%
  # Some have two classes: public and foreign-controlled (changes suffix from 1 to 3)
  mutate(name = if_else(is.na(organisation), name, NA_character_),
         `start-date` = NA, `end-date` = NA) %>%
  separate(classes, c("one", "two"), sep = "/", fill = "right") %>%
  mutate(two = if_else(is.na(two), two, paste0(str_sub(one, end = -2), "3"))) %>%
  unite(classes, one, two, sep = ";") %>%
  mutate(classes = str_replace_all(classes, ";NA", "")) %>%
  mutate(classes = if_else(classes == "NA", NA_character_, classes)) %>%
  rename(`public-body-classifications` = classes) %>%
  write_tsv(dest_file, na = "")

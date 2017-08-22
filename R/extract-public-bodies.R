library(tidyverse)
library(readxl)
library(here)

source_file <- file.path(here(), "lists", "list.xls")
dest_file <- file.path(here(), "lists", "public-body.csv")

read_excel(source_file, sheet = 8, range = "A5:C4853") %>%
  select(Institutions, `ESA10 category`) %>%
  rename("name" = Institutions, "esa-2010" = `ESA10 category`) %>%
  write_csv(dest_file)

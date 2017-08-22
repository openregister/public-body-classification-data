# Download the latest file from ONS, replacing any other files in /lists

library(tidyverse)
library(rvest)
library(lubridate)
library(here)

home_url <- "https://www.ons.gov.uk/economy/nationalaccounts/uksectoraccounts/datasets/publicsectorclassificationguide"

home <-
  home_url %>%
  read_html()

file_date <-
  home %>%
  html_node(".margin-bottom-md--0") %>%
  html_text() %>%
  paste("1") %>%
  myd()

file_url <-
  home %>%
  html_node(".btn--thick") %>%
  html_attr("href") %>%
  paste0("https://www.ons.gov.uk", .)

dest_path <- file.path(here(), "lists", paste0("list.xls"))

file.remove(list.files(file.path(here(), "lists"), full.names = TRUE))

download.file(file_url, dest_path)

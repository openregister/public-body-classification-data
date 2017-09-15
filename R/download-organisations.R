# Download and combine the various registers of public bodies,
# currently:
# * government-organisation
# * local-authority-eng
# * local-authority-sct
# * principal-local-authority

library(RegistersClientR)
library(tidyverse)
library(here)

org <-
  rr_records("government-organisation") %>%
  mutate(organisation = paste0("government-organisation:", `government-organisation`))
eng <-
  rr_records("local-authority-eng") %>%
  mutate(organisation = paste0("local-authority-eng:", `local-authority-eng`))
sct <-
  rr_records("local-authority-sct") %>%
  mutate(organisation = paste0("local-authority-sct:", `local-authority-sct`))
wls <-
  rr_records("principal-local-authority") %>%
  mutate(organisation = paste0("principal-local-authority:", `principal-local-authority`))

bind_rows(eng, sct, wls) %>%
  select(`official-name`, organisation, `start-date`, `end-date`) %>%
  rename("name" = `official-name`) %>%
  bind_rows(select(org, name, organisation)) %>%
  write_csv(file.path(here(), "lists", "organisation.csv"))

library(tidyverse)
library(readxl)
library(here)

source_file <- file.path(here(), "lists", "list-corrected.xls")
dest_file <- file.path(here(), "lists", "public-body.csv")

# WARNING: Some dates had to be manually fixed in the spreadsheet

# 1: In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
#   Expecting date in D1815 / R1815C4: got '27-Jun-1876'
# 2: In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
#   Expecting date in D2499 / R2499C4: got 'Since the start of the scheme'
# 3: In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
#   Expecting date in D2976 / R2976C4: got 'Various'
# 4: In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
#   Expecting date in D3595 / R3595C4: got 'Jun-06 - Aug-10'
# 5: In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
#   Expecting date in D3596 / R3596C4: got '2008-2010'
# 6: In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
#   Expecting date in D4236 / R4236C4: got 'Various'
# 7: In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
#   Expecting date in D4503 / R4503C4: got 'Various'

# All the dates were blanked because, either:
# * they were 'en bloc', which won't be imported anyway
# * they were 'former' with no match in orgs, so won't be imported anyway
# * the date is so long ago that it might as well be blank for "since ever"

read_excel(source_file, sheet = 8, range = "A5:D4853") %>%
  select(Institutions,
         Sector,
         `ESA10 category`,
         `Classification applies from`) %>%
  rename(name = Institutions,
         sector = Sector,
         `esa-2010` = `ESA10 category`,
         `start-date` = `Classification applies from`) %>%
  write_csv(dest_file)

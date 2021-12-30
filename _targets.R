# _targets.R

library(targets)
library(tarchetypes)
library(here)

source(here("scripts", "my-setup.R"))
source(here("scripts", "functions.R"))

options(tidyverse.quiet = TRUE)
options(dplyr.summarise.inform = FALSE)
options(tigris_use_cache = TRUE)

# tar_option_set(
#   # TODO: do I need to set "packages =" here if I load them in my-setup.R earlier?
#   packages = c("tidyverse", "arrow", "jsonlite",  "janitor",  "gt",  "scales",  "glue",
#                "lubridate", "purrr", "doParallel",  "hrbrthemes") #,
# )

list(
  tar_target(facilities_all, get_facilities()),
  tar_target(park_years_usa, get_park_years_usa(facilities_all)),
  #tar_target(nc_facilities, get_nc_facilities(facilities_all)),
  tar_target(path_nc_history_files, prepare_nc_history()),
  tar_target(nc_camping_history, get_nc_camping_history(path_feather = path_nc_history_files, 
                                                        facilities = facilities_all)),
  tar_target(nc_camping_history_detail, get_nc_camping_history_dow_detail(nc_camping_history)),
  tar_target(nc_campsites_from_history, get_nc_campsites_from_nc_camping_history(nc_camping_history)),
  tar_target(nc_camping_history_yday, get_nc_camping_history_yday(nc_camping_history)),
  tar_target(federal_orgs, get_federal_orgs()),
  NULL
)

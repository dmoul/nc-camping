# my-setup.R

knitr::opts_chunk$set(echo = FALSE)

library(tidyverse, warn.conflicts = FALSE)
library(arrow, warn.conflicts = FALSE)
library(jsonlite)
library(janitor)
library(gt)
library(scales)
library(glue)
library(lubridate)
library(purrr)

library(sf) # for map
library(tigris) # for counties() boundaries used in map
library(ggthemes) # for theme_map()
library(ggrepel) # for geom_text_repel()
library(patchwork)
library(hrbrthemes) # for theme_ipsum_ps() and import_plex_sans()
## remotes::install_github("hrbrmstr/hrbrthemes")
## run 2021-08-01 to address segfault errors like this:
## sh: line 1: 11020 Segmentation fault: 11  '/Library/Frameworks/R.framework/Versions/4.1/Resources/library/Rttf2pt1/exec//ttf2pt1' -a -GfAe '/Library/Frameworks/R.framework/Versions/4.1/Resources/library/hrbrthemes/fonts/roboto-condensed/RobotoCondensed-Bold.ttf' '/var/folders/0j/hjw8sflx19l6rw4s1l68qq640000gn/T//RtmpGN3PSJ/fonts/RobotoCondensed-Bold' 2>&1
theme_set(theme_ipsum_ps() + 
            theme(panel.grid.minor = element_blank()))
import_plex_sans()

library(targets)
library(tarchetypes)

options(dplyr.summarise.inform = FALSE)
options(tigris_use_cache = TRUE)

# constants
big_campground_size <- 50 # fifty or more campsites
summer_season_start = "2021-05-15"
summer_season_end = "2021-09-15"
dow_labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
year_lower_bound <- 2009 # first year with "steady-state" data

# let's make some stuff go faster 
doParallel::registerDoParallel(cores = parallel::detectCores() - 1) 

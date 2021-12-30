# functions.R

###### get_facilities ###### 

get_facilities <- function(path_json_fac = "./data/raw/RIDBFullExport_V1_JSON/Facilities_API_v1.json",
                           path_json_fac_addr = "./data/raw/RIDBFullExport_V1_JSON/FacilityAddresses_API_v1.json") {
  # create df with list of US facilities
  
  # test
  # path_json <- "./data/raw/RIDBFullExport_V1_JSON/Facilities_API_v1.json"
  # path_json_fac_addr = "./data/raw/RIDBFullExport_V1_JSON/FacilityAddresses_API_v1.json"
  
  facility_addresses_raw <- fromJSON(path_json_fac_addr,
                                     flatten = FALSE)
  
  facility_addresses <- facility_addresses_raw[[1]] %>%
    clean_names() %>%
    filter(facility_address_type == "Physical") %>%
    distinct(facility_id, .keep_all = TRUE) %>%
    select(facility_id, facility_state = postal_code)
  # note: there are 122 facilities without a state designation. 
  
  facilities_raw <- fromJSON(path_json_fac,
                             flatten = FALSE)
  facilities_raw[[1]] %>%
    clean_names() %>%
    select(-c(legacy_facility_id, 
              facility_description, # might be interesting to keep, but this is a big field so drop it for now)
              facility_use_fee_description, # has some info about camping (individual / group) that might be helpful
              facility_directions:facility_ada_access,
              geojson,
              stay_limit,
              enabled)
    ) %>%
    left_join(.,
              facility_addresses,
              by = "facility_id") %>%
    mutate(keywords = str_to_lower(keywords),
           facility_name = str_to_title(facility_name),
           facility_type_description = str_to_lower(facility_type_description))
  
}


###### get_federal_orgs ######

get_federal_orgs <- function(path_json = "./data/raw/RIDBFullExport_V1_JSON/Organizations_API_v1.json") {
  
  orgs_raw <- fromJSON(path_json,
                       flatten = FALSE)
  
  orgs_raw[[1]] %>%
    clean_names() %>%
    select(org_id, org_name, org_abbrev_name, org_url_address)
  
}


###### get_nc_campsites_from_facilities ######
#   # WARNING: campsite list in json file seems woefully incomplete
#   # Better to use get_nc_campsites_from_nc_camping_history()
#
# get_nc_campsites_from_facilities <- function(facilities, path_json_campsites = "./data/raw/RIDBFullExport_V1_JSON/Campsites_API_v1.json") {
#   
#   nc_facilities <- facilities %>%
#     filter(facility_state == "NC") %>%
#     distinct(facility_id, facility_name)
#   
#   campsites_raw <- fromJSON(path_json_campsites,
#                             flatten = FALSE)
#   campsites <- campsites_raw[[1]] %>%
#     clean_names() %>%
#     mutate(type_of_use = str_to_lower(type_of_use),
#            campsite_type = str_to_lower(campsite_type)) %>%
#     left_join(nc_facilities, .,
#               by = "facility_id") %>%
#     filter(type_of_use != "day") %>%
#     arrange(facility_name, campsite_name, campsite_id)
#   
# }


###### get_nc_campsites_from_nc_camping_history ######

get_nc_campsites_from_nc_camping_history <- function(nc_camping_history) {
  
  # test
  # nc_camping_history <- nc_history
  
  nc_camping_history %>%
    distinct(facility_id, facility_name, product_id, .keep_all = TRUE) %>%
    rename(campsite_id = product_id) %>%
    arrange(facility_name, campsite_id)
  
  # campsite list in json file seems woefully incomplete
  
  # nc_facilities <- facilities %>%
  #   filter(facility_state == "NC") %>%
  #   distinct(facility_id, facility_name)
  # 
  # campsites_raw <- fromJSON(path_json_campsites,
  #                           flatten = FALSE)
  # campsites <- campsites_raw[[1]] %>%
  #   clean_names() %>%
  #   mutate(type_of_use = str_to_lower(type_of_use),
  #          campsite_type = str_to_lower(campsite_type)) %>%
  #   left_join(nc_facilities, .,
  #             by = "facility_id") %>%
  #   filter(type_of_use != "day") %>%
  #   arrange(facility_name, campsite_name, campsite_id)
  
}

###### get_park_years ###### 

get_park_years_usa <- function(facilities, path_csv = "./data/raw/reservations/") {
  # create df with list of all facilities that provide overnight camping and the first and last year reservations were offered in the dataset
  
  get_park_years <- function(fname) {
    
    # test
    # fname <- "./data/raw/reservations/2020.csv"
    
    message("working on ", fname)
    
    tbl_tmp <- read_csv(file = fname, 
                        col_types = cols(.default = "c"),
                        id = "path") %>%
      clean_names()
    
    tbl_tmp2 <- tbl_tmp
    
    if(any(str_detect(tbl_tmp2$path, "2019|2020"))) {
      tbl_tmp2 <- tbl_tmp2 %>%
        select(-starts_with(c("entityid", "equipment", "discount")))
      
      if(any(str_detect(tbl_tmp2$path, "2020"))) {
        tbl_tmp2 <- tbl_tmp2 %>% 
          select(-nights)
      }
      
      names(tbl_tmp2) <- c("path", "historical_reservation_id", "order_number", "agency", "org_id",
                           "code_hierarchy", "region_code", "region_description", "parent_location_id", "parent_location",
                           "legacy_facility_id", "park", "site_type", "use_type", "product_id",
                           "entity_type", "facility_id", "facility_zip", "facility_state",
                           "facility_longitude", "facility_latitude", "customer_zip", "customer_state", "customer_country",
                           "tax", "use_fee", "tran_fee", "attr_fee", "total_before_tax",
                           "total_paid", "start_date", "end_date", "order_date", "number_of_people")
      
    }
    
    tbl_tmp2 %>%
      # head(100000) %>% # for testing purposes
      select(path, agency, org_id, site_type:facility_state, ends_with("date")) %>%
      left_join(.,
                facilities %>% 
                  select(-c(facility_state, reservable, last_updated_date)),
                by = "facility_id") %>%
      # drop items that don't include overnight reservations
      filter(!is.na(facility_name), # drop some bad data
             !is.na(end_date), # drop things we know aren't really overnight camping (some tours are overnight, for example)
             use_type == "Overnight") %>%
      mutate(across(ends_with("date"), ~ as.Date(.x, origin = "1970-01-01")),
             start_year = year(start_date),
             order_year = year(order_date)) %>%
      distinct(facility_id, facility_name, site_type, start_year, order_year, .keep_all = TRUE) %>%
      mutate(across(ends_with(c("tude", "_people")), as.numeric),
             facility_name = str_to_title(facility_name),
             facility_type_description = str_to_lower(facility_type_description),
             site_type = str_to_lower(site_type),
             entity_type = str_to_lower(entity_type),
             use_type = str_to_lower(use_type),
             # some are missing facility_state values. These are the ones in NC:
             facility_state = ifelse(facility_name %in% c("Lake Powhatan Glamping", "Deep Creek Campground (Nc)", "Crabtree Falls Campground"), 
                                      "NC", 
                                      facility_state)
      ) %>%
      mutate(facility_state = case_when(
        str_length(facility_state) == 2  ~ facility_state,
        str_length(facility_state) > 2   ~ state.abb[match(facility_state, state.name)], # where full state name, convert to abbreviation
        TRUE                             ~ NA_character_
      )) %>%
      replace_na(list(facility_state = "(not_specified)"))
    
  }
  
  # Now do the work
  
  # test
  # path_csv <- "./data/raw/reservations/"
  # all_files_csv_hist <- "./data/raw/reservations/2020.csv"
  
  all_files_csv_hist <- fs::dir_ls(path = path_csv,
                                   recurse = FALSE,
                                   type = "file",
                                   regexp = "[.]csv$"
  )
  
  park_years_raw <- map_dfr(all_files_csv_hist, get_park_years)
  
  park_years <- park_years_raw  %>%
    # select(-geometry) %>% # feather is not handling sf geometry properties correctly, so I'll recreate it later
    rename(site_type_original = site_type) %>%
    mutate(
      site_type_original = str_to_lower(site_type_original),
      site_type_original = ifelse(site_type_original == "campsite", "standard", site_type_original),
      site_type = case_when(
        str_detect(site_type_original, "equestrian")                ~ "equestrian", # needs to be before "group"
        str_detect(site_type_original, "group")                     ~ "group",
        str_detect(site_type_original, "standard")                  ~ "tent", # true in all or nearly all cases
        str_detect(site_type_original, "tent")                      ~ "tent",
        str_detect(site_type_original, "cabin|yurt")                ~ "cabin",
        str_detect(site_type_original, "rv")                        ~ "rv",
        str_detect(site_type_original, "management")                ~ "management",
        str_detect(site_type_original, "shelter|lookout")           ~ "shelter",
        str_detect(site_type_original, "hike|lookout|walk")         ~ "standard",
        str_detect(site_type_original, "anchorage|boat|mooring")    ~ "other",
        str_detect(site_type_original, "no|yes")                    ~ "other",
        str_detect(site_type_original, "picnic|zone")               ~ "other",
        TRUE                                                        ~ "other"
      )
    ) %>%
    distinct(facility_id, site_type, start_year, .keep_all = TRUE)
  
  #write_feather(park_years, paste0("./data/processed/park-years.feather"))
  saveRDS(park_years, paste0("./data/processed/park-years.rds"))
  
  park_years
  
}


###### prepare_nc_history ###### 

prepare_nc_history <- function(path_csv = "./data/raw/reservations/",
                               path_feather = "./data/processed/nc_by_year_semiprocessed/") {
  # INPUT: set of CSV files in path_csv with USA reservation history, one file per year
  # OUTPUT creates feather files with NC history, one per year year
  #        return location of feather files, which is path_feather
  
  get_nc_historical <- function(fname) {
    # INPUT: one CSV file
    # OUTPUT: df with NC historical reservations from CSV file 
    
    # test
    # fname <- "./data/raw/reservations/2010.csv"
    
    tbl_tmp <- read_csv(file = fname,
                        col_types = cols(.default = "c"),
                        id = "path") %>%
      clean_names()
    
    tbl_tmp2 <- tbl_tmp %>%
      # fix NC missing faciility_state values so we can work with all NC reservations
      # `park` will become `facility_name` later
      mutate(facility_state = if_else(park %in% str_to_upper(c("Crabtree Falls Campground", "Deep Creek Campground (Nc)", "Lake Powhatan Glamping")), 
                                      "NC", 
                                      facility_state)) %>%
      filter(facility_state == "NC") %>%
      mutate(across(ends_with("date"), ~ as.Date(.x, origin = "1970-01-01"))) %>%
      mutate(across(ends_with(c("tude", "_people")), as.numeric)) %>%
      select(1:number_of_people) %>%
      mutate(nights = as.numeric(difftime(end_date, start_date, units = "days"))) %>%
      filter(nights > 0) # drop day use shelters
    
    tbl_tmp2 %>%
      select(-facility_state, -facility_zip) 
  }
  
  get_nc_historical_2019plus <- function(fname) {
    # INPUT: one CSV file
    # OUTPUT: df with NC historical reservations from CSV file 
    
    # test
    # fname <- "./data/raw/reservations/2020.csv"
    
    tbl_tmp <- read_csv(file = fname,
                        col_types = cols(.default = "c"),
                        id = "path") %>%
      clean_names() 
    
    tbl_tmp2 <- tbl_tmp %>%
      filter(facilitystate == "North Carolina",
             inventorytype != "VEHICLE_PERMIT") %>% # not off-road permits
      mutate(across(ends_with("date"), ~ as.Date(.x, origin = "1970-01-01"))) %>%
      mutate(across(ends_with(c("tude", "people")), as.numeric)) %>%
      select(1:numberofpeople, - starts_with(c("entityid", "equipment", "discount"))) %>% #some don't exist pre-2019
      mutate(nights = as.numeric(difftime(enddate, startdate, units = "days"))) %>%
      filter(nights > 0)  # drop day use shelters
    
    # entity_type replaces inventorytype; removed "entity_id", 
    names(tbl_tmp2) <- c("path", "historical_reservation_id", "order_number", "agency", "org_id",
                         "code_hierarchy", "region_code", "region_description", "parent_location_id", "parent_location",
                         "legacy_facility_id", "park", "site_type", "use_type", "product_id",
                         "entity_type", "facility_id", "facility_zip", "facility_state",
                         "facility_longitude", "facility_latitude", "customer_zip", "customer_state", "customer_country",
                         "tax", "use_fee", "tran_fee", "attr_fee", "total_before_tax",
                         "total_paid", "start_date", "end_date", "order_date", "number_of_people", "nights")
    
    tbl_tmp2 %>%
      select(-facility_state, -facility_zip) 
  }
  
  create_nc_historical_feather_files <- function(path_feather = "./data/processed/nc_by_year_semiprocessed/") {
    ###### Create feather files, one per year of NC reservations
    
    # test
    # path_feather  = "./data/processed/nc_by_year_semiprocessed/"
    
    for(i in 2006:2018) { # 2006:2018
      message("working on ", i)
      tbl <- get_nc_historical(all_files_csv_hist[str_detect(all_files_csv_hist, as.character(i))])
      write_feather(tbl, paste0(path_feather, "nc_", i, ".feather"))
    }
    
    ## The above ran 2021-11-14 with these warnings (I believe they are not relevant):
    # working on 2010
    # Warning: One or more parsing issues, see `problems()` for details
    # working on 2011
    # Warning: One or more parsing issues, see `problems()` for details
    # working on 2012
    # Warning: One or more parsing issues, see `problems()` for details
    # working on 2013
    # Warning: One or more parsing issues, see `problems()` for details
    # working on 2014
    # Warning: One or more parsing issues, see `problems()` for details
    # working on 2015
    # Warning: One or more parsing issues, see `problems()` for details
    # Warning in doTryCatch(return(expr), name, parentenv, handler) :
    #   restarting interrupted promise evaluation
    # working on 2016
    # Warning: One or more parsing issues, see `problems()` for details
    # working on 2017
    # Warning: One or more parsing issues, see `problems()` for details
    # working on 2018
    # Warning: One or more parsing issues, see `problems()` for details
    
    for(i in 2019:2020) {
      
      # test
      # i <- 2020
      # path_feather  = "./data/processed/nc_by_year_semiprocessed/"
      
      message("working on ", i)
      tbl <- get_nc_historical_2019plus(all_files_csv_hist[str_detect(all_files_csv_hist, as.character(i))])
      write_feather(tbl, paste0(path_feather, "nc_", i, ".feather"))
    }
  }
  
  ### Now do the work
  
  # test
  # path_csv <- "./data/raw/reservations/"
  
  all_files_csv_hist <- fs::dir_ls(path = path_csv,
                                   recurse = FALSE,
                                   type = "file",
                                   regexp = "[.]csv$"
  )
  
  create_nc_historical_feather_files(path_feather)
  
  # return the location of the feather files
  path_feather
  
}

###### get_nc_camping_history ###### 

get_nc_camping_history <- function(path_feather = "./data/processed/nc_by_year_semiprocessed/",
                                   facilities) {
  
  # test
  # path_feather = "./data/processed/nc_by_year_semiprocessed/"
  # facilities = tar_read(facilities_all)
  
  all_files <- fs::dir_ls(path = path_feather,
                          recurse = FALSE,
                          type = "file",
                          regexp = "[.]feather$"
  )
  
  nc_hist_tmp <- map_dfr(all_files, read_feather)
  
  nc_hist_tmp %>%
    select(-c(starts_with(c("historical", "legacy")), region_code, code_hierarchy, park)) %>% #, starts_with("parent")
    mutate_if(is.character, ~map_chr(.x, iconv, "UTF-8", "UTF-8", sub='')) %>% # just in case
    mutate(site_type = str_to_lower(site_type),
           entity_type = str_to_lower(entity_type),
           use_type = str_to_lower(use_type),
    ) %>% 
    filter(use_type == "overnight") %>%
    mutate(entity_type = if_else(entity_type == "site", "camping", entity_type), # assume "site" means "camping"
           across(tax:total_paid, as.numeric),
           person_nights  = number_of_people * nights,
           paid_person_night = total_paid / person_nights
    ) %>%
    inner_join(.,
               facilities %>% select(-c(facility_longitude, facility_latitude, facility_state, reservable, last_updated_date)),
               by = "facility_id") %>%
    rename(site_type_original = site_type) %>%
    mutate(facility_name = str_to_title(facility_name),
           facility_type_description = str_to_lower(facility_type_description),
           site_type = case_when(
             str_detect(site_type_original, "equestrian")                ~ "equestrian", # needs to be before "group"
             str_detect(site_type_original, "group")                     ~ "group",
             str_detect(site_type_original, "standard")                  ~ "tent", # true in all or nearly all cases
             str_detect(site_type_original, "tent")                      ~ "tent",
             str_detect(site_type_original, "cabin|yurt")                ~ "cabin",
             str_detect(site_type_original, "rv")                        ~ "rv",
             str_detect(site_type_original, "management")                ~ "management",
             str_detect(site_type_original, "shelter|lookout")           ~ "shelter",
             str_detect(site_type_original, "hike|lookout|walk")         ~ "standard",
             str_detect(site_type_original, "anchorage|boat|mooring")    ~ "other",
             str_detect(site_type_original, "no|yes")                    ~ "other",
             str_detect(site_type_original, "picnic|zone")               ~ "other",
             TRUE                                                        ~ "other"
           )
    ) %>%
    select(path, facility_id, facility_name, site_type, entity_type, use_type, 
           start_date, end_date, number_of_people, nights, person_nights, tidyselect::everything())
  
  # write_feather(nc_hist, paste0("./data/processed/nc_historical_camping.feather")) not needed since target will cache it
  
}


###### get_nc_camping_history_dow ###### 
## Replaced by get_nc_camping_history_dow_detail
# get_nc_camping_history_dow <- function(tbl) {
#   # INPUT: df with nc_camping_history
#   # OUTPUT: much larger df with one row for each day of the week each reservation included
#   
#   tbl %>%
#     filter(nights > 0) %>%
#     #head(1000) %>% # for testing
#     mutate(all_days = map2(start_date, nights, ~ seq(from = .x, 
#                                                      to = .x + .y -1, 
#                                                      by = 1)
#     ),
#     days_of_reservation = map(all_days, ~ wday(.x, label = TRUE, abbr = TRUE)),
#     all_days_string = map_chr(days_of_reservation, glue_collapse, sep = ", "),
#     season = if_else(between(yday(start_date),
#                              yday(summer_season_start),
#                              yday(summer_season_end)
#     ),
#     "summer-season",
#     "off-season"
#     )
#     ) %>%
#     select(start_date, nights, all_days, all_days_string, days_of_reservation, season) %>%
#     separate_rows(all_days_string) %>%
#     mutate(all_days_string = factor(all_days_string, levels = dow_labels)) 
#   
# }

###### get_nc_camping_history_dow_detail ###### 

get_nc_camping_history_dow_detail <- function(tbl) {
  # INPUT: df with nc_camping_history
  # OUTPUT: much larger df with one row for each day of the week each reservation included
  
  # test
  # tbl <- nc_history
  
  tbl %>%
    filter(nights > 0) %>%
    # head(10000) %>% # for testing
    select(-c(path, entity_type, use_type, agency:parent_location, customer_zip:total_before_tax, org_facility_id:keywords)) %>%
    mutate(all_days = map2(start_date, nights, ~ seq(from = .x, 
                                                     to = .x + .y -1, 
                                                     by = 1)
    ),
    days_of_reservation = map(all_days, ~ wday(.x, label = TRUE, abbr = TRUE)),
    all_days_string = map_chr(days_of_reservation, glue_collapse, sep = ", "),
    all_days_number_string = map_chr(all_days, glue_collapse, sep = ", "),
    season = if_else(between(yday(start_date),
                             yday(summer_season_start),
                             yday(summer_season_end)
    ),
    "summer-season",
    "off-season"
    )
    ) %>%
    # select(facility_name, facility_id, site_type, start_date, nights, all_days, all_days_string, all_days_number_string, days_of_reservation, season) %>%
    separate_rows(all_days_number_string, sep =  ", ") %>% #, all_days_string
    mutate(overnight_date = as.Date(all_days_number_string),
           day_label = wday(overnight_date, label = TRUE),
           loc_mountains = if_else(facility_longitude < -80.5, 1, 0), # valid for NC only
           loc_coast = if_else(facility_longitude > -77.5, 1, 0), # valid for NC only
    ) %>%
    select(-c(all_days, all_days_number_string))
  
}

get_nc_camping_history_yday <- function(tbl) {
  # INPUT: df with nc_camping_history
  # OUTPUT: much larger df with one row for each day of the week each reservation included
  
  # test
  # tbl <- nc_history
  
  tbl %>%
    filter(nights > 0) %>%
    mutate(year = year(start_date)) %>%
    # for testing
    # filter(year == 2017#,
    #        #facility_name == "Badin Lake Campground"
    #        ) %>% # limit data for testing
    # head(10) %>% 
    mutate(all_days = map2(start_date, nights, ~ seq(from = .x, 
                                                     to = .x + .y -1, 
                                                     by = 1)
    ),
    yday_of_reservation = map(all_days, yday), 
    days_of_reservation = map(all_days, ~ wday(.x, label = TRUE, abbr = TRUE)),
    all_days_string = map_chr(days_of_reservation, glue_collapse, sep = ", "),
    week_of_reservation = map(yday_of_reservation, ~ floor(.x / 7) + 1),
    week_string = map_chr(week_of_reservation, glue_collapse, sep = ", "),
    season = if_else(between(yday(start_date),
                             yday(summer_season_start),
                             yday(summer_season_end)
                             ),
                     "summer",
                     "off-season"
                     ),
    row_id = row_number()
    ) %>%
    select(row_id, year, facility_name, site_type, start_date, nights, 
           all_days, days_of_reservation, yday_of_reservation, week_of_reservation, week_string, season)
  
}


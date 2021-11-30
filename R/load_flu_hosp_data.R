#' Obtain influenza signal at daily or weekly scale
#'
#' @param as_of Date or string in format "YYYY-MM-DD" specifying the date which the data was available on or before.
#'   If null, the default returns the most recent available data.
#' @param locations optional list of FIPS or location abbreviations. Defaults to the US, the 50 states, DC, PR, and VI.
#' @param temporal_resolution "daily" or "weekly"
#' @param na.rm boolean indicating whether NA values should be dropped when aggregating state-level values and
#'   calculating weekly totals. Defaults to `TRUE`
#’
#’ @return data frame of flu incidence with columns date, location, location_name, value
   
load_flu_hosp_data <- function(as_of = NULL, locations = "*", temporal_resolution, na.rm = TRUE) {
  library(covidcast)
  library(tidyverse)
  options(dplyr.summarise.inform = FALSE)
  # load location data
  location_data <- readr::read_csv(file = "data/locations.csv",show_col_types = FALSE) %>%
    dplyr::mutate(geo_value=tolower(abbreviation)) %>%
    dplyr::select(-c("population","abbreviation"))
  # get geo_value based on fips if fips are provided
  if (any(grepl("\\d", locations))){
    locations <- location_data$geo_value[which(location_data$location %in% locations)]
  } else {
    locations <- tolower(locations) 
  }
  # pull daily state data
  state_dat <- covidcast::covidcast_signal(
    as_of = as_of,
    geo_values = locations,
    data_source = "hhs",
    signal = "confirmed_admissions_influenza_1d",
    geo_type = "state") %>%
    dplyr::mutate(
      epiyear = lubridate::epiyear(time_value),
      epiweek = lubridate::epiweek(time_value)
      ) %>%
    dplyr::select(geo_value, epiyear, epiweek, time_value, value) %>%
    dplyr::rename(date = time_value)
    
  # creating US and bind to state-level data if US is specified or locations
  if(locations=="*"||("us" %in% locations)){
    US_dat <- state_dat %>%
      group_by(epiyear, epiweek, date) %>%
      summarize(value = sum(value, na.rm=na.rm)) %>%
      dplyr::mutate(geo_value="us") %>%
      dplyr::ungroup() %>%
      dplyr::select(geo_value, epiyear, epiweek, date, value) 
    # bind to daily data
    dat <- rbind(US_dat,state_dat) 
  } else {
    dat <- state_dat
  }
  # weekly aggregation
  if(temporal_resolution!="daily"){
    dat <- dat %>%
      dplyr::group_by(epiyear, epiweek, geo_value) %>%
      dplyr::summarize(
        date = max(date),
        num_days = n(),
        value = sum(value, na.rm = na.rm)
      ) %>%
      dplyr::filter(num_days == 7L) %>%
      dplyr::ungroup() %>%
      dplyr::select(-"num_days") 
  } 
  final_data <- dat %>%
    dplyr::left_join(location_data,by="geo_value") %>%
    dplyr::select(date, location, location_name, value)
  return(final_data)
}
#' Title load_fluHosp_truth
#'
#' @param target_variable column name in epidata, default to "previous_day_admission_influenza_confirmed"
#' @param as_of the date of retrieval or version date, default to current date
#' @param truth_date a single or a set of truth dates, default to the most recent Monday 
#' @param locations an abbreviation of a state name or a set of abbreviations of state names, default to all states
#'
#' @return a data frame with a column  for each parameter
load_fluHosp_truth <- function(
                       target_variable = NULL,
                       as_of = NULL,
                       truth_date = NULL,
                       locations = NULL){
  # libraries
  library(tidyverse)
  library(lubridate)
  library(purrr)
  library(data.table)
  # for now default source to epidata
  library(httr)
  
  # Because the API is stateless, the Epidata class only contains static methods
  Epidata <- (function() {
    
    # API base url
    BASE_URL <- 'https://delphi.cmu.edu/epidata/api.php'
    
    client_version <- '0.3.0'
    
    # Helper function to cast values and/or ranges to strings
    .listitem <- function(value) {
      if(is.list(value) && 'from' %in% names(value) && 'to' %in% names(value)) {
        return(paste0(toString(value$from), '-', toString(value$to)))
      } else {
        return(toString(value))
      }
    }
    
    # Helper function to build a list of values and/or ranges
    .list <- function(values) {
      if(!is.list(values) || ('from' %in% names(values) && 'to' %in% names(values))) {
        values <- list(values)
      }
      return(paste(sapply(values, .listitem), collapse=','))
    }
    
    # Helper function to request and parse epidata
    .request <- function(params) {
      # API call
      res <- GET(BASE_URL, query=params)
      if (res$status_code == 414) {
        res <- POST(BASE_URL, body=params, encode='form')
      }
      return(content(res, 'parsed'))
    }
    
    # Build a `range` object (ex: dates/epiweeks)
    range <- function(from, to) {
      if(to <= from) {
        temp <- to
        to <- from
        from <- temp
      }
      return(list(from=from, to=to))
    }
    
    # Fetch COVID hospitalization data
    covid_hosp <- function(states, dates, issues) {
      # Check parameters
      if(missing(states) || missing(dates)) {
        stop('`states` and `dates` are both required')
      }
      # Set up request
      params <- list(
        endpoint = 'covid_hosp',
        states = .list(states),
        dates = .list(dates)
      )
      if(!missing(issues)) {
        params$issues <- .list(issues)
      }
      # Make the API call
      return(.request(params))
    }
    
    # Export the public methods
    return(list(
      range = range,
      client_version = client_version,
      covid_hosp = covid_hosp
    ))
  })()
  #  specify params
  if(missing(locations)){
    locs <- c("AL", "AK", "AZ", 
              "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", 
              "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", 
              "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", 
              "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", 
              "WA", "WV", "WI", "WY", "AS", "GU", "MP", "PR", "UM", "VI", "DC")
  }
  else{
    locs <- locations
  }
  if(missing(truth_date)){
    # set to the most recent monday
    tdates <- format(floor_date(Sys.Date(), 'week')+1,format="%Y%m%d")
  }else{
    tdates <- format(as.Date(truth_date),format="%Y%m%d")
  }
  if(missing(as_of)){
    # set to today
    issue_date <-  NULL
  }else{
    issue_date <- format(as.Date(as_of),format="%Y%m%d")
  }
  # format truth dates
  # get data
  list_results <- lapply(locs, function(x) Epidata$covid_hosp(x,tdates)[['epidata']]) %>%
     do.call("c",.) %>%
     lapply(., function(x) data.frame(t(unlist(x)))) %>%
     purrr::map_dfr(., ~ .x %>%
               as.list %>%
               as_tibble)
  #check if  empty
  if(nrow(list_results)==0){
    warning("Empty truth data, please check your parameters.")
  }
  # second round of filtering
  if(missing(target_variable)){
    tar <- "previous_day_admission_influenza_confirmed"
  }else{
    tar <- target_variable
  }
  # have to give an warning if the column does not exist
  #for particular dates & locs or user should just check?
  truth_data <- list_results%>%
   dplyr::select(c("state","issue","date",all_of(tar)))
  return(truth_data )
}


# adapted from weekly-submission/fit_baseline_model.R
library(covidcast)
library(epitools)
library(dplyr)
library(tidyr)
library(ggplot2)
library(simplets)
library(covidHubUtils)
setwd(here())
source("./R/load_flu_hosp_data.R")
source("./R/fit_baseline_one_location.R")

# Set locations and quantiles
required_quantiles <-c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)  
required_locations <- readr::read_csv(file = "./data/locations.csv") %>%
  dplyr::select("location","abbreviation")
# Figure out what day it is; forecast creation date is set to a Monday,
# even if we are delayed and create it Tuesday morning.
forecast_week_end_date <- as.character(
  lubridate::floor_date(Sys.Date(), unit = "week") - 1
)
# Load data
data <- load_flu_hosp_data(as_of=forecast_week_end_date) %>%
  dplyr::left_join(required_locations, by="location") %>%
  dplyr::mutate(geo_value=tolower(abbreviation)) %>%
  dplyr::select(geo_value, time_value = date, value) 

location_number <- length(required_locations$abbreviation)
# set variation of baseline to fit
transformation_variation <- c("none","sqrt")
symmetrize_variation <- c(TRUE, FALSE)
window_size_variation <- 28
# fit baseline models
forecast_week_end_date <- lubridate::ymd(forecast_week_end_date)
quantile_forecasts <- purrr::map_dfr(required_locations$abbreviation,
                               function(location){
                                 data <- data %>%
                                   dplyr::filter(geo_value == tolower(location)) %>%
                                   tidyr::drop_na()
                                 location_results <- fit_baseline_one_location(forecast_week_end_date, data, 
                                                                               transformation_variation, 
                                                                               symmetrize_variation, 
                                                                               window_size_variation, 
                                                                               required_quantiles)
                                 
                               }
) %>%
  dplyr::left_join(required_locations, by = "abbreviation") %>%
  dplyr::select(forecast_date, target, target_end_date, location, type, quantile, value, model)

model_number <- length(unique(quantile_forecasts$model))
model_names <- unique(quantile_forecasts$model)
results_paths <- paste0('weekly-submission/forecasts/FluSight-', model_names, '/',
                       forecast_week_end_date + 2,
                       '-FluSight-',model_names,'.csv')
for(i in 1:model_number){
  model_name <- model_names[i]
  # set path 
  results_path <- results_paths[i]
  model_forecasts <-  quantile_forecasts %>%
    dplyr::filter(model == model_name) %>%
    dplyr::select(-"model")
  write.csv(model_forecasts, file = results_path, row.names = FALSE)
}

      
#-------------------------------------------------------------------------------------------------------------------------------#
# plot forecasts and save as pdfs to respective folders
# f <- covidHubUtils::load_forecasts_repo(
#         file_path= 'weekly-submission/forecasts/',
#         models = NULL,
#         forecast_dates = "2021-12-13",
#         locations = c("US","06"),
#         types = NULL,
#         targets = NULL,
#         hub = "FluSight",
#         verbose = TRUE
#       )
# covidHubUtils::plot_forecasts(forecast_data = f, hub = "FluSight",facet="location", truth_source = "HealthData") 



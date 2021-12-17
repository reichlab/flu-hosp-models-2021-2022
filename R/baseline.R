# adapted from weekly-submission/fit_baseline_model.R
library(covidcast)
library(epitools)
library(dplyr)
library(tidyr)
library(ggplot2)
library(simplets)
library(covidHubUtils)
library(ggforce)
# library(here)
# setwd(here())
source("./R/load_flu_hosp_data.R")
source("./R/fit_baseline_one_location.R")

# Set locations and quantiles
required_quantiles <-
  c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
required_locations <-
  readr::read_csv(file = "./data/locations.csv") %>%
  dplyr::select("location", "abbreviation")
# The reference_date is the date of the Saturday relative to which week-ahead targets are defined.
# The forecast_date is the Monday of forecast creation.
# The forecast creation date is set to a Monday,
# even if we are delayed and create it Tuesday morning.
reference_date <- as.character(lubridate::floor_date(Sys.Date(), unit = "week") - 1)
forecast_date <- as.character(as.Date(reference_date) + 2)
# Load data
data <- load_flu_hosp_data(as_of = forecast_date) %>%
  dplyr::left_join(required_locations, by = "location") %>%
  dplyr::mutate(geo_value = tolower(abbreviation)) %>%
  dplyr::select(geo_value, time_value = date, value)

location_number <- length(required_locations$abbreviation)
# set variation of baseline to fit
transformation_variation <- c("none", "sqrt")
symmetrize_variation <- c(TRUE, FALSE)
window_size_variation <- 28
# fit baseline models
reference_date <- lubridate::ymd(reference_date)
quantile_forecasts <-
  purrr::map_dfr(required_locations$abbreviation,
                 function(location) {
                   data <- data %>%
                     dplyr::filter(geo_value == tolower(location))
                   location_results <-
                     fit_baseline_one_location(
                       reference_date,
                       data,
                       transformation_variation,
                       symmetrize_variation,
                       window_size_variation,
                       required_quantiles
                     )
                   
                 }) %>%
  dplyr::left_join(required_locations, by = "abbreviation") %>%
  dplyr::select(forecast_date,
                target,
                target_end_date,
                location,
                type,
                quantile,
                value,
                model)

model_number <- length(unique(quantile_forecasts$model))
model_names <- unique(quantile_forecasts$model)
model_folders <-
  paste0('/UMassCoE-',
         model_names,
         '/',
         forecast_date,
         '-UMassCoE-',
         model_names)
results_paths <-
  paste0('weekly-submission/forecasts', model_folders, '.csv')
plot_paths <-
  paste0('weekly-submission/baseline-plots', model_folders, '.pdf')
for (i in 1:model_number) {
  model_name <- model_names[i]
  # set path
  results_path <- results_paths[i]
  model_forecasts <-  quantile_forecasts %>%
    dplyr::filter(model == model_name) %>%
    dplyr::select(-"model")
  write.csv(model_forecasts, file = results_path, row.names = FALSE)
  # plot
  plot_path <- plot_paths[i]
  f <- covidHubUtils::load_forecasts_repo(
    file_path = paste0('weekly-submission/forecasts/'),
    models = paste0('UMassCoE-', model_names[i]),
    forecast_dates = forecast_date,
    locations = NULL,
    types = NULL,
    targets = NULL,
    hub = "FluSight",
    verbose = TRUE
  )
  p <-
    covidHubUtils::plot_forecasts(
      forecast_data = f,
      facet = "~location",
      hub = "FluSight",
      truth_source = "HealthData",
      subtitle = "none",
      title = "none",
      show_caption = FALSE,
      plot = FALSE
    ) +
    scale_x_date(
      breaks = "1 month",
      date_labels = "%b-%y",
      limits = as.Date(c(
        reference_date - (7 * 32), reference_date + 28
      ), format = "%b-%y")
    ) +
    theme_update(
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      axis.text.x = element_text(angle = 90),
      axis.title.x = element_blank()
    ) +
    ggforce::facet_wrap_paginate(
      ~ location,
      scales = "free",
      ncol = 2,
      nrow = 3,
      page = 1
    )
  n <- n_pages(p)
  pdf(
    plot_path,
    paper = 'A4',
    width = 205 / 25,
    height = 270 / 25
  )
  for (i in 1:n) {
    suppressWarnings(print(
      p + ggforce::facet_wrap_paginate(
        ~ location,
        scales = "free",
        ncol = 2,
        nrow = 3,
        page = i
      )
    ))
  }
  dev.off()
}



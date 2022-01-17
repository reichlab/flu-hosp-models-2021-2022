# adapted from weekly-submission/fit_baseline_model.R
library(covidcast)
library(epitools)
library(dplyr)
library(tidyr)
library(ggplot2)
library(simplets)
library(covidHubUtils)
library(hubEnsembles)
library(ggforce)
library(furrr)
# library(here)
# setwd(here())
source("./R/load_flu_hosp_data.R")
source("./R/fit_baseline_one_location.R")

ncores <- future::availableCores()
future::plan(multisession, workers = ncores - 1)

# Set locations and quantiles
required_quantiles <-
  c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
required_locations <-
  readr::read_csv(file = "./data/locations.csv") %>%
  dplyr::select("location", "abbreviation")

# The reference_date is the date of the Saturday relative to which week-ahead targets are defined.
# The forecast_date is the Monday of forecast creation.
# This cannot be run on a later date unless source is set to covidcast with as_of == forecast_date
reference_date <-
  as.character(lubridate::floor_date(Sys.Date(), unit = "week") - 1)
forecast_date <- as.character(as.Date(reference_date) + 2)

# Load data
daily_data <- load_flu_hosp_data(
    as_of = NULL,
    temporal_resolution = "daily") %>%
  dplyr::left_join(required_locations, by = "location") %>%
  dplyr::mutate(geo_value = tolower(abbreviation)) %>%
  dplyr::select(geo_value, time_value = date, value)

weekly_data <- load_flu_hosp_data(
    as_of = NULL,
    temporal_resolution = "weekly") %>%
  dplyr::left_join(required_locations, by = "location") %>%
  dplyr::mutate(geo_value = tolower(abbreviation)) %>%
  dplyr::select(geo_value, time_value = date, value)

location_number <- length(required_locations$abbreviation)

# set up variations of baseline to fit
model_variations <- dplyr::bind_rows(
  tidyr::expand_grid(
    transformation = c("none", "sqrt"),
    symmetrize = c(TRUE, FALSE),
    window_size = c(4, 3),
    temporal_resolution = "weekly"
  ),
  tidyr::expand_grid(
    transformation = c("none", "sqrt"),
    symmetrize = c(FALSE),
    # symmetrize = c(TRUE, FALSE),
    window_size = c(21, 14),
    temporal_resolution = "daily"
  )
)

# fit baseline models
reference_date <- lubridate::ymd(reference_date)
quantile_forecasts <-
  furrr::future_pmap_dfr(
    model_variations,
    function(transformation,
             symmetrize,
             window_size,
             temporal_resolution) {
      purrr::map_dfr(
        required_locations$abbreviation,
        function(location) {
          if (temporal_resolution == "daily") {
            loc_data <- daily_data %>%
              dplyr::filter(geo_value == tolower(location))
          } else {
            loc_data <- weekly_data %>%
              dplyr::filter(geo_value == tolower(location))
          }
          location_results <- fit_baseline_one_location(
            reference_date = reference_date,
            location_data = loc_data,
            transformation = transformation,
            symmetrize = symmetrize,
            window_size = window_size,
            temporal_resolution = temporal_resolution,
            taus = required_quantiles
          )
        }) %>%
      dplyr::left_join(required_locations, by = "abbreviation") %>%
      dplyr::select(
        forecast_date,
        target,
        target_end_date,
        location,
        type,
        quantile,
        value,
        model)
    })

model_number <- nrow(model_variations)
model_names <-
  c(unique(quantile_forecasts$model), "trends_ensemble")
model_folders <- file.path(
  "weekly-submission/forecasts",
  paste0("UMass-", model_names)
)

plots_folders <- file.path(
  "weekly-submission/plots",
  paste0("UMass-", model_names)
)

for (folder in c(model_folders, plots_folders)) {
  if (!file.exists(folder)) dir.create(folder, recursive = TRUE)
}

results_paths <- file.path(
  model_folders,
  paste0(
    forecast_date,
    "-UMass-",
    model_names,
    ".csv"))

plot_paths <- file.path(
  plots_folders,
  paste0(
    forecast_date,
    "-UMass-",
    model_names,
    ".pdf"))

# save all the baseline models in hub format
for (i in 1:model_number) {
  model_name <- model_names[i]
  # set path
  results_path <- results_paths[i]
  model_forecasts <-  quantile_forecasts %>%
    dplyr::filter(model == model_name) %>%
    dplyr::select(-"model")
  write.csv(model_forecasts, file = results_path, row.names = FALSE)
}

# load them back in to a single data.frame having columns required by
# build_quantile_ensemble and plot_forecasts
all_baselines <- covidHubUtils::load_forecasts_repo(
  file_path = paste0("weekly-submission/forecasts/"),
  models = paste0("UMass-", model_names[1:model_number]),
  forecast_dates = forecast_date,
  locations = NULL,
  types = NULL,
  targets = NULL,
  hub = "FluSight",
  verbose = TRUE
)

# build ensemble
baseline_ensemble <- hubEnsembles::build_quantile_ensemble(
  all_baselines,
  forecast_date = forecast_date,
  model_name = "baseline_ensemble"
) %>%
  dplyr::mutate(
    value = ifelse(
      is.na(quantile) | quantile >= 0.5, ceiling(value), floor(value)
    ))

# save ensemble in hub format
write.csv(
  baseline_ensemble %>% dplyr::transmute(
    forecast_date = forecast_date,
    target = paste(horizon, temporal_resolution, "ahead", target_variable),
    target_end_date = target_end_date,
    location = location,
    type = type,
    quantile = quantile,
    value = value
  ),
  file = results_paths[model_number + 1],
  row.names = FALSE
)

# load ensemble back in and bind with other baselines for plotting
all_baselines <- dplyr::bind_rows(
  all_baselines,
  covidHubUtils::load_forecasts_repo(
    file_path = paste0("weekly-submission/forecasts/"),
    models = "UMass-trends_ensemble",
    forecast_dates = forecast_date,
    locations = NULL,
    types = NULL,
    targets = NULL,
    hub = "FluSight",
    verbose = TRUE
  )
)

truth_for_plotting <- weekly_data %>%
  dplyr::mutate(abbreviation = toupper(geo_value)) %>%
  dplyr::left_join(hub_locations_flusight, by = "abbreviation") %>%
  dplyr::transmute(
    model = "Observed Data (HealthData)",
    location = fips,
    target_end_date = time_value,
    target_variable = "inc flu hosp",
    value = value)

plot_results <- furrr::future_map_lgl(
  seq_len(model_number + 1),
  function(i) {
    # plot
    plot_path <- plot_paths[i]
    p <-
      covidHubUtils::plot_forecasts(
        forecast_data = all_baselines %>%
          dplyr::filter(model == paste0('UMass-',model_names[i])),
        facet = "~location",
        hub = "FluSight",
        truth_data = truth_for_plotting,
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
    for (page_num in 1:n) {
      suppressWarnings(print(
        p + ggforce::facet_wrap_paginate(
          ~ location,
          scales = "free",
          ncol = 2,
          nrow = 3,
          page = page_num
        )
      ))
    }
    dev.off()
    return(TRUE)
  })

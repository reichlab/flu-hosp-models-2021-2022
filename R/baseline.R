# library(devtools)
# install.packages("feasts")
# install_github("reichlab/simplets")
# install_github("reichlab/hubEnsembles")

# adapted from weekly-submission/fit_baseline_model.R
library(covidcast)
library(epitools)
library(dplyr)
library(tidyr)
library(ggplot2)
library(simplets)
library(covidHubUtils)
library(hubEnsembles)
library(hubUtils)
library(ggforce)
library(furrr)
# library(here)
# setwd(here())
# source("./R/load_flu_hosp_data.R")
source("./R/fit_baseline_one_location.R")
source("./R/as_scorable_forecasts.R")

ncores <- future::availableCores()
future::plan(multisession, workers = ncores - 1)

# Set locations and quantiles
required_quantiles <-
  c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
required_locations <-
  readr::read_csv(file = "./data/locations.csv") %>%
  dplyr::select("location", "abbreviation")

# The reference_date is the date of the Saturday relative to which week-ahead targets are defined.
# aka due_date (a wednesday) + 4, the Saturday of the current week
# The due_date is the Wednesday of forecast creation and the day forecasts must be submitted.

# This cannot be run on a later date unless source is set to covidcast with as_of == forecast_date
reference_date <-
  as.character(lubridate::ceiling_date(Sys.Date(), unit = "week") - 1)
due_date <- as.character(as.Date(reference_date) - 4)

# Load data
daily_data <- covidData::load_data(
#  as_of = due_date,
  spatial_resolution = c("national", "state"),
  temporal_resolution = "daily",
  measure = "flu hospitalizations",
  drop_last_date = TRUE
) %>%
  dplyr::left_join(covidData::fips_codes, by = "location") %>%
  dplyr::transmute(
    date,
    location,
    location_name = ifelse(location_name == "United States", "US", location_name),
    value = inc) %>%
  dplyr::filter(location != "60") %>%
  dplyr::arrange(location, date) %>%
  # the previous lines reproduce the output of the current `load_flu_hosp_data` function
  # the following lines currently follow the call to `load_flu_hosp_data` in baseline.R
  dplyr::left_join(required_locations, by = "location") %>%
  dplyr::mutate(geo_value = tolower(abbreviation)) %>%
  dplyr::select(geo_value, time_value = date, value)

weekly_data <- covidData::load_data(
  spatial_resolution = c("national", "state"),
  temporal_resolution = "weekly",
  measure = "flu hospitalizations",
  drop_last_date = FALSE
) %>%
  dplyr::left_join(covidData::fips_codes, by = "location") %>%
  dplyr::transmute(
    date,
    location,
    location_name = ifelse(location_name == "United States", "US", location_name),
    value = inc) %>%
  dplyr::filter(location != "60") %>%
  dplyr::arrange(location, date) %>%
  # the previous lines reproduce the output of the current `load_flu_hosp_data` function
  # the following lines currently follow the call to `load_flu_hosp_data` in baseline.R
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
    # transformation = "sqrt",
    # symmetrize = c(FALSE),
    symmetrize = c(TRUE, FALSE),
    window_size = c(21 - 1, 14 - 1),
    # window_size = 7,
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
        reference_date,
        horizon,
        target,
        target_end_date,
        location,
        output_type,
        output_type_id,
        value,
        model_id)
    })

model_number <- nrow(model_variations)
model_names <-
  c(unique(quantile_forecasts$model_id), "trends_ensemble")
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
    reference_date,
    "-UMass-",
    model_names,
    ".csv"))

plot_paths <- file.path(
  plots_folders,
  paste0(
    reference_date,
    "-UMass-",
    model_names,
    ".pdf"))

# save all the baseline models in hub verse format
for (i in 1:model_number) {
  model_name <- model_names[i]
  # set path
  results_path <- results_paths[i]
  model_forecasts <- quantile_forecasts %>%
    dplyr::filter(model_id == model_name) %>%
    dplyr::select(-"model_id")
  write.csv(model_forecasts, file = results_path, row.names = FALSE)
}


# build ensemble
baseline_ensemble <- quantile_forecasts |>
  as_model_out_tbl() |>
  hubEnsembles::simple_ensemble(
    agg_fun="median",
    model_id = "UMass-trends_ensemble"
  ) |>
  dplyr::mutate(
#    value = ifelse(is.na(output_type_id) | output_type_id >= 0.5, ceiling(value), floor(value)),
  reference_date = as.Date(reference_date), target_end_date = as.Date(target_end_date)
  )


# save ensemble in hub format
#baseline_ensemble |>
#  dplyr::select(-"model_id") |>
#  write.csv(file=results_paths[model_number + 1], row.names=FALSE)
  
# bind all (baseline) models together and transform into CovidHubUtils format for plotting
all_baselines <- quantile_forecasts |>
  as_model_out_tbl() |>
  dplyr::bind_rows(baseline_ensemble) |>
  dplyr::mutate(
    reference_date=as.Date(reference_date) - lubridate::weeks(2), 
    horizon=as.character(horizon+2)
  ) |>
  as_scorable_forecasts(reference_date_col="reference_date", temp_res_col=NULL) |>
  dplyr::left_join(covidHubUtils::hub_locations_flusight, by=c("location"="fips"))
  
forecasts_for_plotting <- all_baselines |>
  dplyr::filter(quantile == 0.5) |>
  dplyr::mutate(type = "point") |>
  dplyr::bind_rows(all_baselines) |>
  dplyr::mutate(target_end_date = as.Date(target_end_date))

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
        forecast_data = forecasts_for_plotting %>%
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

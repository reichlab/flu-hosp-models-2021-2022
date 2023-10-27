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
library(lubridate)
# library(here)
# setwd(here())
source("./R/fit_baseline_one_location.R")
source("./R/as_covid_hub_forecasts.R")

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
  drop_last_date = FALSE # changed from TRUE 
) %>%
  dplyr::left_join(covidData::fips_codes, by = "location") %>%
  dplyr::transmute(
    date=date+days(1),
    location,
    location_name = ifelse(location_name == "United States", "US", location_name),
    value = inc) %>%
  dplyr::filter(location %in% pull(required_locations, location)) %>%
  dplyr::arrange(location, date) %>%
  # the previous lines reproduce the output of the current `load_flu_hosp_data` function
  # the following lines currently follow the call to `load_flu_hosp_data` in baseline.R
  dplyr::left_join(required_locations, by = "location") %>%
  dplyr::mutate(geo_value = tolower(abbreviation)) %>%
  dplyr::select(geo_value, time_value = date, value)

last_truth_saturday <- daily_data |>
  dplyr::distinct(time_value, .keep_all = TRUE) |>
  dplyr::slice_max(time_value, n = 7) |>
  dplyr::mutate(day = lubridate::wday(time_value, label=TRUE, abbr=TRUE)) |>
  dplyr::filter(day=="Sat") |>
  dplyr::pull(time_value)

weekly_data <- daily_data |>
  dplyr::filter(time_value <= last_truth_saturday) |>
  dplyr::mutate(associated_saturday = lubridate::ceiling_date(time_value, "week") - days(1)) |>
  dplyr::group_by(geo_value, associated_saturday) |>
  dplyr::summarize(week_value=sum(value)) |>
  dplyr::transmute(geo_value, time_value=associated_saturday, value=week_value)

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
  c(unique(quantile_forecasts$model_id), "UMass-trends_ensemble")
model_folders <- file.path("weekly-submission/forecasts", model_names)

plots_folders <- file.path("weekly-submission/plots", model_names)

for (folder in c(model_folders, plots_folders)) {
  if (!file.exists(folder)) dir.create(folder, recursive = TRUE)
}

results_paths <- file.path(
  model_folders,
  paste0(
    reference_date,
    "-",
    model_names,
    ".csv"))

plot_paths <- file.path(
  plots_folders,
  paste0(
    reference_date,
    "-",
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
  dplyr::mutate(reference_date = as.Date(reference_date), target_end_date = as.Date(target_end_date)) |>
  as_model_out_tbl() |>
  dplyr::bind_rows(baseline_ensemble) |>
  dplyr::mutate(reference_date=reference_date - lubridate::weeks(1), horizon=as.character(horizon+2)) |>
#  dplyr::mutate(reference_date=reference_date - lubridate::weeks(2), horizon=as.character(horizon+2)) |>
  as_covid_hub_forecasts(reference_date_col="reference_date", temp_res_col=NULL) |>
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
  seq_len(model_number),
  function(i) {
    plot_path <- plot_paths[i]
   p <-
      covidHubUtils::plot_forecasts(
        forecast_data = forecasts_for_plotting %>%
          dplyr::filter(model == model_names[i]),
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


library(covidHubUtils)
library(distfromq)
library(reshape2)
library(ggplot2)
library (ggforce)
library(tidyverse)
library(readr)
library(dplyr)
library(gridExtra)
library (furrr)

#models = c("UMass-trends_ensemble"); source = "local_hub_repo";  hub_repo="../flu-hosp-models-2021-2022";
#data_processed="./weekly-submission/forecasts/";  c_target="wk flu hosp rate change";  output1="../Flusight-forecast-data/data-experimental/";
#output2="../flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/"; output3="../flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/plots/"

#Important dates used
# last_eval_sat <- as.Date(calc_target_week_end_date(Sys.Date(), horizon = 0))
last_eval_sat <- as.Date(calc_target_week_end_date(Sys.Date(), horizon = 0))-7 # for debugging if run Tuesday-Sunday
# last_eval_sat <- as.Date(calc_target_week_end_date(Sys.Date(), horizon = 0))-14 # for out-of-season
this_monday=last_eval_sat+2
prior_eval_sat=last_eval_sat-7
prior_10wk_eval_sat=last_eval_sat-70

## load location data
location_data <- readr::read_csv(file = "../Flusight-forecast-hub/auxiliary-data/locations.csv") |>
  dplyr::mutate(geo_value = tolower(abbreviation)) |>
  dplyr::select(-abbreviation, -5)

weekly_data_all <- weekly_data |>
  dplyr::rename(target_end_date = time_value) |>
  dplyr::ungroup() |>
  tidyr::expand(target_end_date, geo_value, horizon = -1:3) |>
  dplyr::left_join(weekly_data, by=c("target_end_date"="time_value", "geo_value"))|>
  dplyr::inner_join(location_data, by = c("geo_value"))  |>
  dplyr::mutate(model_id="Observed Data (HealthData)", target_variable="inc flu hosp", .before=1) |>

  #  CRIT1 Large increase: positive forecasted rate changes larger than or equal to LI/100k,
  # where LI = {2,3,4,5,5} AND the count change is larger than 10+LI/100k.
  mutate(crit1=case_when(horizon == -1 & count_rate2 >= 10 ~ value+count_rate2,
                         horizon == -1 & count_rate2 < 10 ~ value + 10 + count_rate2,
                         horizon == 0 & count_rate3 >= 10 ~ value + count_rate3,
                         horizon == 0 & count_rate3 < 10 ~ value + 10 + count_rate3,
                         horizon == 1 & count_rate4 >= 10 ~ value + count_rate4,
                         horizon == 1 & count_rate4 < 10 ~ value + 10 + count_rate4,
                         horizon >= 2 & count_rate5 >= 10 ~ value + count_rate5,
                         horizon >= 2 & count_rate5 < 10 ~ value + 10 + count_rate5)) %>%

  #  CRIT2 Increase: positive forecasted rate changes larger than or equal to I/100k,
  # where I = {1,1,2,2.5,2.5} AND the count change is larger than 10.
  mutate(crit2=case_when(horizon < 2 & count_rate1 >= 10 ~ value+count_rate1,
                         horizon < 2 & count_rate1 < 10 ~ value + 10,
                         horizon == 2 & count_rate2 >= 10 ~ value + count_rate2,
                         horizon == 2 & count_rate2 < 10 ~ value + 10,
                         horizon >= 2 & count_rate2p5 >= 10 ~ value + count_rate2p5,
                         horizon >= 2 & count_rate2p5 < 10 ~ value + 10)) %>%

  #  CRIT3 Decrease: Negative forecasted rate changes larger than or equal to D/100k,
  # where D = {1,1,2,2.5,2.5} AND the count change is larger than 10.
  mutate(crit3=case_when(horizon < 2 & count_rate1 >= 10 ~ value - count_rate1,
                         horizon < 2 & count_rate1 < 10 ~ value - 10,
                         horizon == 2 & count_rate2 >= 10 ~ value - count_rate2,
                         horizon == 2 & count_rate2 < 10 ~ value - 10,
                         horizon >= 2 & count_rate2p5 >= 10 ~ value - count_rate2p5,
                         horizon >= 2 & count_rate2p5 < 10 ~ value - 10)) %>%

  #  CRIT4 Large Decrease: Negative forecasted rate changes larger than or equal to LI/100k,
  # where LD = {2,3,4,5,5} AND the count change is larger than 10+LD/100k.
  mutate(crit4=case_when(horizon == -1 & count_rate2 >= 10 ~ value - count_rate2,
                         horizon == -1 & count_rate2 < 10 ~ value - 10 - count_rate2,
                         horizon == 0 & count_rate3 >= 10 ~ value - count_rate3,
                         horizon == 0 & count_rate3 < 10 ~ value - 10 - count_rate3,
                         horizon == 1 & count_rate3 >= 10 ~ value - count_rate3,
                         horizon == 1 & count_rate3 < 10 ~ value - 10 - count_rate4,
                         horizon >= 2 & count_rate5 >= 10 ~ value - count_rate5,
                         horizon >= 2 & count_rate5 < 10 ~ value - 10 - count_rate5)) %>%

  dplyr::select(model_id, location_name, location, value, target_end_date, horizon,target_variable, population, count_rate1:count_rate5, crit1, crit2, crit3, crit4) |>
  dplyr::filter(!is.na(value))

  #filter most recent truth
recent_date <- ifelse(last_eval_sat %in% unique(weekly_data$time_value), last_eval_sat, max(weekly_data$time_value))
weekly_data_recent <-  weekly_data_all %>%
  filter(target_end_date == recent_date) %>%
  mutate(date = reference_date)  %>%
  dplyr::select(location, location_name, date, horizon, data_value=value, population, count_rate1:count_rate5, crit1, crit2, crit3, crit4)

# list of locations
the_locations <- weekly_data_recent %>%
  distinct(location, .keep_all=TRUE) %>%
  pull(location) #states, us and territories

# extract log pdf and cdf values for training set forecasts
# we add a little noise to the value column so that there is a density to
# work with in case the forecaster had a point mass anywhere
train_forecasts <- baseline_ensemble %>%
  dplyr::mutate(
    reference_date=as.Date(reference_date),
    value = rnorm(n = nrow(baseline_ensemble), mean = value, sd = 0.1)
  ) %>%
  dplyr::inner_join(weekly_data_recent,
                    by = c("reference_date" = "date", "horizon", "location")) %>%
  dplyr::group_by(model_id, reference_date, location, location_name, horizon, target, target_end_date) %>%
  dplyr::summarize(
    cdf_crit1 = distfromq::make_p_fn(
      ps = output_type_id,
      qs = value)(unique(crit1), log = FALSE),
    cdf_crit2 = distfromq::make_p_fn(
      ps = output_type_id,
      qs = value)(unique(crit2), log = FALSE),
    cdf_crit3 = distfromq::make_p_fn(
      ps = output_type_id,
      qs = value)(unique(crit3), log = FALSE),
    cdf_crit4 = distfromq::make_p_fn(
      ps = output_type_id,
      qs = value)(unique(crit4), log = FALSE),
  )

  #calculate percentages, correcting for negative numbers
  exp_forecast <- train_forecasts  %>%
    ungroup()  %>%
    dplyr::inner_join(weekly_data_recent,
                      by = c("reference_date" = "date", "location_name", "location", "horizon"))  %>%
    mutate(large_increase=1-cdf_crit1,
           increase=cdf_crit1-cdf_crit2,
           stable=case_when(crit3>0  ~ cdf_crit2-cdf_crit3,
                            crit3<=0 ~ cdf_crit2),
           decrease=case_when(crit3>0 & crit4 > 0 ~ cdf_crit3-cdf_crit4,
                              crit3>0 & crit4 <= 0 ~ cdf_crit3),
           large_decrease=case_when(crit4>0 ~ cdf_crit4)) %>%
    select(model_id,reference_date,location,location_name, horizon, large_increase,increase,stable,decrease,large_decrease)

  #transpose data_frame to format for submission
  exp_t = melt(exp_forecast,id.vars = c("model_id","reference_date","location","location_name","horizon"),measure.vars = c("large_increase","increase","stable","decrease","large_decrease") ,
               variable.name="output_type_id", value.name="value")
  exp_t <-exp_t %>%
    mutate(target="wk flu hosp rate change", output_type="pmf", value=ifelse(is.na(value), 0, value))  %>%
    select(model_id, reference_date, horizon, target, location, location_name, output_type, output_type_id, value)

#########################
# output submission data
#########################

trends_ensemble_submission <- exp_t |>
  dplyr::select(-location_name) |>
  dplyr::mutate(
    output_type_id=as.character(output_type_id),
    target_end_date=reference_date+weeks(horizon)
  ) |>
  dplyr::bind_rows(mutate(baseline_ensemble, output_type_id=as.character(output_type_id)))|>
  dplyr::filter(location %in% pull(required_locations, location)) |>
  dplyr::select(reference_date, horizon, target, target_end_date, location, output_type, output_type_id, value)

readr::write_csv(trends_ensemble_submission,results_paths[17])

#######################################################
# Plot original forecasts and Experimental forecasts
#######################################################

#open PDF
pdf(file=plot_paths[17],paper='a4r')
# pdf(file=destination)
for (i in 1:length(the_locations)) {
  p1<-plot_forecasts(forecast_data=forecasts_for_plotting,
                     hub = 'FluSight',
                     truth_data=filter(truth_for_plotting, target_end_date>=prior_10wk_eval_sat),
                     location = the_locations[i],
                     target_variable="inc flu hosp",
                     models=model_names[17],
                     # facet = ~model, facet_scales = "fixed", title = "default",
                     show_caption = TRUE,
                     truth_source="HealthData",
                     title='Inc Flu Hosp',
                     plot=FALSE) +
    theme(legend.position = c(.05,.95), legend.justification = c(0,1), legend.key = element_rect(colour = "transparent", fill = "white"),
          legend.background = element_rect(alpha("white", 0.5)), legend.box="horizontal")

    exp_t_1 <-exp_t  %>%
      filter(location== the_locations[i])

    p2 <- ggplot(exp_t_1, aes(fill=output_type_id, y=value, x=horizon)) +
      geom_bar(position="stack", stat="identity") +
      labs(title='Flu Hosp Rate Change')

    grid.arrange(p1,p2,nrow=1, widths = c(5,3))
  }
  dev.off()


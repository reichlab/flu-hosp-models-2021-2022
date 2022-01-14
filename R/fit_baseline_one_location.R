#' Fit a baseline model  for one location
#'
#' Get quantile function
#'
#' @param  predictions baseline predictions
#' @param  taus probability levels
#'
get_quantiles_df <- function(predictions, taus) {
  n <- length(taus)
  purrr::map_dfr(1:4,
                 function(h) {
                   data.frame(
                     h = rep(h, n),
                     quantile = taus,
                     value = ceiling(quantile(predictions[, h], probs = taus))
                   )
                 })
}
#'
#' Get predictions
#'
#' @param  location_data data frame containing flu hospitalizations for a single location
#'   after outlier correction.
#' @param  response_var a value column after outlier detection and correction.
#' @param  transformation can be either "none" or "sqrt" or  both.
#' @param  symmetrize can be either `TRUE` or `FALSE` or both.
#' @param  window_size a value or a vector of values of window size.
#' @param  horizons horizons to predict at
#' @param  temporal_resolution 'daily' or 'weekly'; specifies timescale of 
#' location_data and horizons
#' @param  h_adjust daily horizon adjustment for aggregation
#'
#' @return data frame of a baseline forecast for one location
get_baseline_predictions <- function(location_data,
                                     response_var,
                                     transformation,
                                     symmetrize,
                                     window_size,
                                     horizons,
                                     temporal_resolution,
                                     h_adjust,
                                     taus) {
  # fit
  baseline_fit <- fit_simple_ts(
    y = location_data[[response_var]],
    ts_frequency = 1,
    model = 'quantile_baseline',
    transformation = transformation,
    transform_offset = ifelse(transformation == "none", 0, 1),
    d = 0,
    D = 0,
    symmetrize = symmetrize,
    window_size = window_size
  )
  
  # predict
  predictions <-
    predict(baseline_fit, nsim = 100000, horizon = horizons)
  
  if (h_adjust < 0) {
    # augment with observed leading data
    daily_predictions <- cbind(
      matrix(
        tail(location_data[[response_var]], abs(h_adjust)),
        nrow = 100000,
        ncol = abs(h_adjust),
        byrow = TRUE),
      daily_predictions
    )
    h_adjust <- 0L
  } else if (h_adjust > 0) {
    # drop extra forecasts at the beginning
    daily_predictions <- daily_predictions[, -seq_len(h_adjust)]
  }
  
  # truncate to non-negative
  predictions <- pmax(predictions, 0)
  
  # aggregate to weekly if temporal_resolution is daily
  ## truncate to start at the first date of the first target week
  temporal_resolution <- match.arg(temporal_resolution, c("daily", "weekly"))
  if (temporal_resolution == "daily") {
    predictions <-
    sapply(1:4, function(i)
      rowSums(daily_predictions[, ((7 * (i - 1)) + 1):(7 * i)])
    )
  }
  # extract predictive quantiles, intervals, and medians
  quantiles_df <- get_quantiles_df(predictions, taus)
  
  return(tibble(quantiles_df = list(quantiles_df)))
}

#' Outlier correction and fitting baseline
#'
#' @param  reference_date the date of the Saturday relative to which week-ahead targets are defined
#' @param  location_data data frame containing flu hospitalizations for a single location. Must contain
#'   geo_value, time_value, and value columns.
#' @param  transformation can be either "none" or "sqrt" or  both.
#' @param  symmetrize can be either `TRUE` or `FALSE` or both.
#' @param  window_size a value or a vector of values of window size.
#' @param  temporal_resolution 'daily' or 'weekly'; specifies timescale of 
#' location_data and horizons
#' @param  taus probability levels
#'
#' @return data frame of a baseline forecast for one location
fit_baseline_one_location <- function(reference_date,
                                      location_data,
                                      transformation,
                                      symmetrize,
                                      window_size,
                                      temporal_resolution,
                                      taus) {
  library(epitools)
  library(dplyr)
  library(tidyr)
  library(simplets)
  library(purrr)
  # convert location data to epi tibble
  location_data = location_data %>%
    suppressWarnings(as.epi_tibble())
  # outlier detection and correction
  detection_methods = bind_rows(
    tibble(
      method = c("rolling_median"),
      method_args = list(list(
        detect_negatives = TRUE,
        detection_multiplier = 2.5
      )),
      method_abbr = c("median")
    ),
    tibble(
      method = c("stl"),
      method_args = list(list(
        detect_negatives = TRUE,
        detection_multiplier = 2.5
      )),
      method_abbr = c("stl_seasonal")
    ),
    tibble(
      method = c("stl"),
      method_args = list(
        list(
          detect_negatives = TRUE,
          detection_multiplier = 2.5,
          include_seasonality = FALSE
        )
      ),
      method_abbr = c("stl_nonseasonal")
    )
  )
  # figure out daily horizons to forecast
  reference_date <- lubridate::ymd(reference_date)
  forecast_date <- reference_date + 2
  last_data_date <- max(location_data$time_value)
  last_target_date <- forecast_date + 28L
  effective_horizon <- as.integer(last_target_date - last_data_date)
  h_adjustments <- as.integer(effective_horizon - 28L)

  # set baseline variations to fit
  variations_to_fit <- tidyr::expand_grid(
    transformation = transformation,
    symmetrize = symmetrize,
    window_size = window_size
  )

  # # tryCatch outlier detection and correction
  # tryCatch({
  #   location_data = location_data %>%
  #     suppressMessages(
  #       detect_outliers(
  #         var = value,
  #         methods = detection_methods,
  #         combine_method = "median",
  #         new_col_name = "outlier_info"
  #       )
  #     ) %>%
  #     suppressMessages(
  #       correct_outliers(
  #         var = value,
  #         outliers_col = "outlier_info",
  #         detection_method = "combined",
  #         new_col_name = "corrected_value1"
  #       )
  #     ) %>%
  #     suppressMessages(
  #       detect_outliers(
  #         var = corrected_value1,
  #         methods = detection_methods,
  #         combine_method = "median",
  #         new_col_name = "outlier_info"
  #       )
  #     ) %>%
  #     suppressMessages(
  #       correct_outliers(
  #         var = corrected_value1,
  #         outliers_col = "outlier_info",
  #         detection_method = "combined",
  #         new_col_name = "corrected_value2"
  #       )
  #     )
  # },
  # error = function(e) {
  #   next
  # },
  # message = function(m) {
  #   next
  # })
  # get predictions
  if ("corrected_value2" %in% colnames(location_data)) {
    var <- "corrected_value2"
  } else{
    var <- "value"
  }
  temporal_resolution <- match.arg(temporal_resolution, c("daily", "weekly"))
  predictions <- purrr::pmap_dfr(
    variations_to_fit,
    get_baseline_predictions,
    location_data = location_data,
    response_var = var,
    horizons = ifelse(temporal_resolution == "daily", effective_horizon, 4),
    temporal_resolution = temporal_resolution,
    h_adjust = h_adjustments,
    taus = taus
  )
  # extract quantile forecasts
  quantiles_df <-
    dplyr::bind_cols(variations_to_fit, predictions) %>%
    tidyr::unnest(quantiles_df) %>%
    dplyr::transmute(
      forecast_date = as.character(forecast_date),
      target = paste0(h, " wk ahead inc flu hosp"),
      target_end_date = as.character(reference_date
                                     +
                                       7L * h),
      abbreviation = toupper(unique(location_data$geo_value)),
      type = 'quantile',
      quantile = quantile,
      value = value,
      model = paste(
        "baseline",
        transformation,
        ifelse(symmetrize, "sym", "nonsym"),
        window_size,
        sep = "_"
      )
    )
  # add point estimates
  quantiles_df <- quantiles_df  %>%
    dplyr::bind_rows(
      .,
      quantiles_df %>%
        dplyr::filter(quantile == 0.5) %>%
        mutate(type = 'point',
               quantile = NA_real_)
    )
  return(quantiles_df)
}

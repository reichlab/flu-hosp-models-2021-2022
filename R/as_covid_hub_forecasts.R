#' Reformat model outputs stored as a `model_output_tbl` class to that of
#' a `data.frame` formatted according to standards of the COVID-19 Forecasting
#' Hub which can be processed by functions from the `covidHubUtils` package
#' such as `score_forecasts()` or `plot_forecasts()`. The supplied 
#' `model_output_tbl` should have columns defining properties akin to 
#' reference dates, locations, horizons, and targets.
#'
#' @param model_outputs an object of class `model_output_tbl` with component
#'   model outputs (e.g., predictions).
#' @param reference_date_col `character` string of the name of the column
#'   containing the reference dates for the forecasts. Defaults to 
#'   "forecast_date".
#' @param location_col `character` string of the name of the column 
#'   containing the locations for the forecasts. Defaults to "location".
#' @param horizon_col `character` string of the name of the column 
#'   containing the horizons for the forecasts. Defaults to "horizon".
#' @param target_col `character` string of the name of the column 
#'   containing the targets for the forecasts. Defaults to "target". If 
#'   `temp_res_col` is NULL, the target column in `model_outputs` is assumed
#'   to contain targets of the form "[temporal resolution] [target]" or 
#'   "[temporal resolution] ahead [target]", such as "wk ahead inc flu hosp"
#'   "wk inc flu hosp".
#' @param temp_res_col `character` string of the name of the column 
#'   containing the temporal resolutions for the forecasts. Defaults to 
#'   "temporal_resolution". Should be set to NULL if no such column exists,
#'   in which case the column will be created from the existing target column.
#' @param target_end_date_col `character` string of the name of the column 
#'   containing the target end dates for the forecasts. Defaults to 
#'   "target_end_date". Should be set to NULL if no such column exists, in
#'   which case the column will be created from the temporal resolution column.
#'
#' @return a `data.frame` of reformatted model outputs that may be fed into 
#'   any of the `covidHubUtils` functions with 10 total columns: model,
#'   forecast_date, location, horizon, temporal_resolution, target_variable,
#'   target_end_date, type, quantile, value.
#' @export
#'
#' @examples
as_covid_hub_forecasts <- function(model_outputs, reference_date_col="forecast_date", location_col="location", horizon_col="horizon", target_col="target", temp_res_col="temporal_resolution", target_end_date_col="target_end_date") {

  model_outputs <- model_outputs |> 
    dplyr::rename(model = model_id, type = output_type, quantile = output_type_id,
                  forecast_date = reference_date_col, location = location_col, target_variable = target_col) 
  
  if (is.null(temp_res_col)) {
    model_outputs <- model_outputs |>
      dplyr::rename(target = target_variable) |>
      mutate(target = ifelse(
        stringr::str_detect(target, "ahead"), 
        stringr::str_replace(target, "ahead", "")|> stringr::str_squish(), 
        target)) |>
      tidyr::separate(target, sep=" ", convert=TRUE, into=c("temporal_resolution", "target_variable"), extra="merge")
  }
  
  if (is.null(target_end_date_col)) {
    model_outputs <- model_outputs |>
      dplyr::mutate(target_end_date=case_when(
        temporal_resolution %in% c("d", "day") ~ forecast_date + lubridate::days(horizon),
        temporal_resolution %in% c("w", "wk", "week") ~ forecast_date + lubridate::weeks(horizon),
        temporal_resolution %in% c("m", "mth", "mnth", "month") ~ forecast_date %m+% months(horizon),
        temporal_resolution %in% c("y", "yr", "year") ~ forecast_date + lubridate::years(horizon),
        .default = forecast_date), 
      .before = type) 
  }
  
  covid_hub_outputs <- model_outputs |>                
    dplyr::select(model, forecast_date, location, horizon, temporal_resolution, 
                  target_variable, target_end_date, type, quantile, value)

  return (covid_hub_outputs)
}

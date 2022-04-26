library(covidHubUtils)
library(ggplot2)
library(dplyr)

first_mon <- as.Date("2022-01-17")
last_mon <- lubridate::floor_date(Sys.Date(), unit = "week") + 1 - 7
fcast_dates <- seq.Date(from = first_mon, to = last_mon, by = 7)
fcasts <- load_forecasts(
  source = "local_hub_repo",
  dates = fcast_dates,
  targets = c(paste0(rep(1:4), " wk ahead inc flu hosp")),
  data_processed_subpath = "data-forecasts/",
  hub_repo_path = "../Flusight-forecast-data/",
  hub = "FluSight")

fcasts %>%
  dplyr::distinct(model, forecast_date)

truth <- load_truth(hub = "FluSight")

scores <- score_forecasts(fcasts,truth = truth,metrics = c("abs_error","wis", "wis_components", "interval_coverage"),use_median_as_point=TRUE)

ggplot(scores,aes(x=model,y=log(wis))) + geom_boxplot() + theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_hline(yintercept = median(log(scores[scores$model == "Flusight-baseline",]$wis))) + facet_wrap(~ horizon)

p <- scores %>%
  dplyr::group_by(model, forecast_date) %>%
  dplyr::summarize(
    wis = mean(wis),
    is_umass_or_ensemble = (model %in% c("UMass-trends_ensemble", "Flusight-ensemble"))
  ) %>%
  ggplot() +
    geom_line(mapping = aes(x = forecast_date, y = wis, color = model, size = factor(is_umass_or_ensemble))) +
    scale_size_manual(values = c(0.5, 3.0)) +
    theme_bw()
p
library(plotly)
ggplotly(p)

scores %>%
  dplyr::group_by(model, forecast_date) %>%
  dplyr::summarize(
    mae = mean(abs_error),
    is_umass_or_ensemble = (model %in% c("UMass-trends_ensemble", "Flusight-ensemble"))
  ) %>%
  ggplot() +
    geom_line(mapping = aes(x = forecast_date, y = mae, color = model, size = factor(is_umass_or_ensemble))) +
    scale_size_manual(values = c(0.5, 3.0)) +
    theme_bw()


scores %>%
  dplyr::group_by(model, forecast_date) %>%
  dplyr::summarize(
    coverage_95 = mean(coverage_95),
    is_umass_or_ensemble = (model %in% c("UMass-trends_ensemble", "Flusight-ensemble"))
  ) %>%
  ggplot() +
    geom_line(mapping = aes(x = forecast_date, y = coverage_95, color = model, size = factor(is_umass_or_ensemble))) +
    geom_hline(yintercept = 0.95) +
    scale_size_manual(values = c(0.5, 3.0)) +
    theme_bw()


scores %>%
  dplyr::group_by(model, forecast_date) %>%
  dplyr::summarize(
    coverage_50 = mean(coverage_50),
    is_umass_or_ensemble = (model %in% c("UMass-trends_ensemble", "Flusight-ensemble"))
  ) %>%
  ggplot() +
    geom_line(mapping = aes(x = forecast_date, y = coverage_50, color = model, size = factor(is_umass_or_ensemble))) +
    geom_hline(yintercept = 0.50) +
    scale_size_manual(values = c(0.5, 3.0)) +
    theme_bw()



scores %>%
  group_by(model) %>%
  summarize(
    wis = mean(wis),
    mae = mean(abs_error),
    coverage_50 = mean(coverage_50),
    coverage_80 = mean(coverage_80),
    coverage_95 = mean(coverage_95),
    wis_dispersion = mean(dispersion),
    wis_over = mean(overprediction),
    wis_under = mean(underprediction)
  ) %>%
  arrange(wis) %>%
  as.data.frame()

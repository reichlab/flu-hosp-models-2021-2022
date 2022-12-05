#' Create experimental targets with categories for "likely increase", "increase", "stable" decrease", "likely decrease"
#'
#' @param hub:	character, which hub to use. Default is "US". Other options are "ECDC" and "FluSight".
#' @param models: Character vector of model abbreviations. Default all models that submitted forecasts meeting the other criteria.
#' @param targets: character vector of targets to retrieve, for example c('1 wk ahead cum death', '2 wk ahead cum death'). 
#' Default to NULL which stands for all valid targets.
#' @param source: string specifying where forecasts will be loaded from: one of "local_hub_repo", "zoltar" and "local_zoltar". Default to "zoltar".
#' @param hub_repo_path: path to local clone of the forecast hub repository
#' @param data_processed_subpath: folder within the hub_repo_path that contains forecast submission files. 
#' Default to "data-processed/", which is appropriate for the covid19-forecast-hub repository.

#'
#' @return exp_forecast:data frame in the format specified by CDC containing new cols with the change categories and their probabilities



library(tidyverse)
library(readr)

# covidHubUtils for loading component model forecast files
# https://github.com/reichlab/covidHubUtils
library(covidHubUtils)

# zeallot perform multiple unpacking, and destructing assignment in R.  
# needed to install disfromq
# https://github.com/nteetor/zeallot

# distfromq for approximating a distribution's pdf and cdf from a collection
# of quantiles
# https://github.com/reichlab/distfromq
library(distfromq)

create_exp_target <- function(hub, models, targets, source, hub_repo_path,data_processed_subpath ) {
  
#Important dates used
last_eval_sat <- as.Date(calc_target_week_end_date(Sys.Date(), horizon = 0))
last_monday=last_eval_sat-5
prior_eval_sat=last_eval_sat-7

# load truth 
#    filter most recent truth
#    set rate, diff, criteria
weekly_data <- load_truth(hub = hub) %>% 
  filter(target_end_date == prior_eval_sat) %>%
  mutate(date = target_end_date + 2) %>%
  mutate(count1 = round(population*0.00001,digits=0), count2=count1*2) %>%

#  CRIT1: Large increase (2_wk_large_increase): positive forecasted rate changes larger than or equal to 2/100k,
# AND the count change is larger than 40. 
  mutate(crit1=case_when(count2 >= 40 ~ value+count2,
                         count2 < 40 ~ value + 40)) %>% 
  
#  CRIT2: Increase (2_wk_increase): positive forecasted rate changes larger than or equal to 1/100k
#  AND count change greater than or equal to 20, but less than 2/100k 
#  OR rate changes larger than or equal to 2/100k AND count change is less than 40.
  mutate(crit2=case_when(count1 >= 20 ~ value+count1,
                         count1 < 20 ~ value + 20)) %>%
  
#  CRIT3: Decrease (2_wk_decrease): negative forecasted rate changes larger than or equal to 1/100k
#  AND count change greater than or equal to 20, but less than 2/100k 
#  OR rate changes larger than or equal to 2/100k AND count change is less than 40.
  mutate(crit3=case_when(count1 >= 20 & value >= count1 ~ value-count1,
                         count1 >= 20 & value <= count1 ~ 0,
                         count1 < 20 & value >= 20 ~ value - 20,
                         count1 < 20 & value < 20 ~ 0)) %>% 
  
#  CRIT4: Large decrease (2_wk_large_decrease): negative forecasted rate changes that have a magnitude larger 
#  than or equal to 2/100k, AND the magnitude of count change is larger than 40.   
  mutate(crit4=case_when(count2 >= 40 & value >= count2 ~ value-count2,
                         count2 >= 40 & value <= count2 ~ 0,
                         count2 < 40 & value >= 40 ~ value - 40,
                         count2 < 40 & value < 40 ~ 0)) %>% 
  
  # dplyr::select(location, date, value) %>%

  dplyr::select(location, date, data_value = value, population, count1, count2, crit1, crit2, crit3, crit4) %>%
    dplyr::filter(!is.na(data_value))

# # load location data (USE THIS WHEN CDC ADDS DATA)
# location_data1 <- readr::read_csv(file = "~/GitHub/Flusight-forecast-data/data-locations/locations1.csv") %>%
#   dplyr::mutate(geo_value = tolower(abbreviation)) %>%
#   dplyr::select(-c("population", "abbreviation"))

# load submission file (2 week ahead)
component_forecast <- 
  load_forecasts(
    models = models,
    dates = last_monday,
    locations = NULL,
    types = c("quantile"),
    targets = targets,
    source = source,
    hub = hub,
    hub_repo_path=hub_repo_path,
    data_processed_subpath=data_processed_subpath)


# extract log pdf and cdf values for training set forecasts
# we add a little noise to the value column so that there is a density to
# work with in case the forecaster had a point mass anywhere
train_forecasts <- component_forecast %>%
  dplyr::mutate(
    value = rnorm(n = nrow(component_forecast), mean = value, sd = 0.1)
  ) %>%
  dplyr::inner_join(weekly_data,
                    by = c("forecast_date" = "date", "location")) %>%
  dplyr::group_by(model, forecast_date, location, horizon,
                  temporal_resolution, target_variable, target_end_date) %>%
  dplyr::summarize(
    cdf_crit1 = distfromq::make_p_fn(
      ps = quantile,
      qs = value)(unique(crit1), log = FALSE),
    cdf_crit2 = distfromq::make_p_fn(
      ps = quantile,
      qs = value)(unique(crit2), log = FALSE),
    cdf_crit3 = distfromq::make_p_fn(
      ps = quantile,
      qs = value)(unique(crit3), log = FALSE),
    cdf_crit4 = distfromq::make_p_fn(
      ps = quantile,
      qs = value)(unique(crit4), log = FALSE),
  )

#calculate percentages
exp_forecast <- train_forecasts  %>%
     mutate(p2_wk_large_increase=1-cdf_crit1,
            p2_wk_increase=cdf_crit1-cdf_crit2,
            p2_wk_stable=cdf_crit2-cdf_crit3,
            p2_wk_decrease=cdf_crit3-cdf_crit4,
            p2_wk_large_decrease=cdf_crit4,
            total=p2_wk_large_increase+p2_wk_increase+p2_wk_stable+p2_wk_decrease+p2_wk_large_decrease) %>%
select(model, forecast_date,location,horizon,temporal_resolution,target_variable,target_end_date,p2_wk_large_increase,p2_wk_increase,p2_wk_stable,p2_wk_decrease,p2_wk_large_decrease)
return(exp_forecast)
}

#example
create_exp_target(hub="FluSight", models = "UMass-trends_ensemble",targets = ("2 wk ahead inc flu hosp"), source = "local_hub_repo", hub_repo_path="~/GitHub/Flusight-forecast-data",
                  data_processed_subpath="./data-forecasts/")

create_exp_target(hub="FluSight", models = "CU-ensemble",targets = ("2 wk ahead inc flu hosp"), source = "local_hub_repo", hub_repo_path="~/GitHub/Flusight-forecast-data",
                  data_processed_subpath="./data-forecasts/")


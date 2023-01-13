#' Create experimental targets with categories for "likely increase", "increase", "stable" decrease", "likely decrease"
#'
#' @param models: Character vector of original model abbreviations. Must be of length 1. 
#' @param source: string specifying where forecasts will be loaded from: one of "local_hub_repo", "zoltar" and "local_zoltar". Default to "local_hub_repo".
#' @param hub_repo: path to local clone of the forecast hub repository
#' @param data_processed: folder within the hub_repo_path that contains forecast submission files. 
#' @param c_target: character vector of targets for experimental submission file.
#' Default to "2 wk flu hosp rate change". 
#' @param output1: path to write csv output file
#' @param output2: 2nd path to write csv output file.  
#' Default to 'NULL' which means no 2nd path to write csv output file.
#' @param output3: path to write plot output file 

#' @return exp_forecast:data frame in the format specified by CDC containing new cols with the change categories and their probabilities
#' @examples \dontrun{suppressMessages(create_exp_target(models = c("UMass-trends_ensemble"),targets = c("2 wk ahead inc flu hosp"), source = "local_hub_repo", hub_repo="~/GitHub/flu-hosp-models-2021-2022",
#' data_processed="./weekly-submission/forecasts/", c_target="2 wk flu hosp rate change", output1="~/GitHub/Flusight-forecast-data/data-experimental/tests/",
#' output2="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/tests/",output3="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/plots/tests/")}
#' 

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

# reshape2 Flexibily reshape data: a reboot of the reshape package
# https://github.com/hadley/reshape
library(reshape2)

# ggplot2 Create elegant data visualization using the grammer of graphics
# https://github.com/tidyverse/ggplot2
library(ggplot2)

# ggforce Accelerating 'ggplot2'
# https://github.com/thomasp85/ggforce
library (ggforce)

# tidyverse: Easily install and load the 'Tidyverse'
# https://github.com/tidyverse/tidyverse
library(tidyverse)

# readr: Read rectangular text data
# https://github.com/tidyverse/readr
library(readr)

# dplyr: A grammar of data manipulation
# https://github.com/tidyverse/dplyr
library(dplyr)

# gridExtra Provides a number of user-level functions to work with "grid" graphics
# https://cran.r-project.org/web/packages/gridExtra
library(gridExtra)

# furrr: Apply Mapping Functions in Parallel using Futures
# https://github.com/DavisVaughan/furrr
library (furrr)



create_exp_target <- function(models,
                              source = "local_hub_repo", 
                              hub_repo,
                              data_processed,
                              c_target = "2 wk flu hosp rate change", 
                              output1, 
                              output2 = NULL,
                              output3) {
  # validate parameter input (models)
  lmodels<-length(models > 1)
  if (lmodels>1) {
    stop("Error models must be length 1")
    return(lmodels)
  }
  
  # validate parameter input (source)
  source <- match.arg(source, choices = c("local_hub_repo", "zoltar", "local_zoltar"))
  
  # validate parameter input (hub_rep)  
  if (source == "local_hub_repo") {
    # validate hub repo path
    if (missing(hub_repo) | !dir.exists(hub_repo)) {
      stop("Error in create_exp_target: Please provide a valid path to hub repo.")
    }
  }
  
  # validate parameter input (hub_rep)  
  if (source == "local_hub_repo") {
    # validate hub repo path
    if (missing(hub_repo) | !dir.exists(hub_repo)) {
      stop("Error in create_exp_target: Please provide a valid path to hub repo.")
    }
  }  
  #Important dates used
  # last_eval_sat <- as.Date(calc_target_week_end_date(Sys.Date(), horizon = 0))
  last_eval_sat <- as.Date(calc_target_week_end_date(Sys.Date(), horizon = 0))-7 # for debugging if run Tuesday-Sunday
  this_monday=last_eval_sat+2
  prior_eval_sat=last_eval_sat-7
  prior_10wk_eval_sat=last_eval_sat-70
  
  # # load location data 
  location_data <- readr::read_csv(file = "~/GitHub/Flusight-forecast-data/data-locations/locations.csv") %>%
    dplyr::mutate(geo_value = tolower(abbreviation)) %>%
    dplyr::select(-c("population", "abbreviation")) %>%
    select(location_name, count_rate1per100k, count_rate2per100k)
  
  # load truth and merge with location data
  #    filter most recent truth
  #    set rate, diff, criteria
  weekly_data_all <- load_truth(hub = 'FluSight',temporal_resolution = "weekly") %>% 
    filter(target_end_date >= prior_10wk_eval_sat) %>%
    dplyr::inner_join(location_data,
                      by = c("location_name"))  %>%
    
    #  CRIT1: Large increase (2_wk_large_increase): positive forecasted rate changes larger than or equal to 2/100k,
    # AND the count change is larger than 40. 
    mutate(crit1=case_when(count_rate2per100k >= 40 ~ value+count_rate2per100k,
                           count_rate2per100k < 40 ~ value + 40)) %>% 
    
    #  CRIT2: Increase (2_wk_increase): positive forecasted rate changes larger than or equal to 1/100k
    #  AND count change greater than or equal to 20, but less than 2/100k 
    #  OR rate changes larger than or equal to 2/100k AND count change is less than 40.
    mutate(crit2=case_when(count_rate1per100k >= 20 ~ value+count_rate1per100k,
                           count_rate1per100k < 20 ~ value + 20)) %>%
    
    #  CRIT3: Decrease (2_wk_decrease): negative forecasted rate changes larger than or equal to 1/100k
    #  AND count change greater than or equal to 20, but less than 2/100k 
    #  OR rate changes larger than or equal to 2/100k AND count change is less than 40.
    mutate(crit3=case_when(count_rate1per100k >= 20  ~ value-count_rate1per100k,
                           count_rate1per100k < 20  ~ value - 20)) %>% 
    
    #  CRIT4: Large decrease (2_wk_large_decrease): negative forecasted rate changes that have a magnitude larger 
    #  than or equal to 2/100k, AND the magnitude of count change is larger than 40.   
    mutate(crit4=case_when(count_rate2per100k >= 40  ~ value-count_rate2per100k,
                           count_rate2per100k < 40  ~ value - 40)) %>% 
    
    # dplyr::select(location, date, value) %>%
    
    dplyr::select(model, location_name, location, value, target_end_date, target_variable, population, count_rate1per100k, count_rate2per100k, crit1, crit2, crit3, crit4) %>%
    dplyr::filter(!is.na(value))
  
  #filter most recent truth
  weekly_data_recent <-  weekly_data_all %>% 
    filter(target_end_date == last_eval_sat) %>%
    mutate(date = target_end_date + 2)  %>%
    dplyr::select(location, location_name, date, data_value = value, population, count_rate1per100k, count_rate2per100k, crit1, crit2, crit3, crit4)
  
  # list of locations
  the_locations <- weekly_data_recent %>%  pull(location) #states, us and territories
  
  # load submission file (2 week ahead)
  component_forecast <- 
    load_forecasts(
      models = models,
      dates = this_monday,
      locations = NULL,
      types = c("quantile","point"),
      targets = paste(1:4, "wk ahead inc flu hosp"),
      source = source,
      hub = "FluSight",
      hub_repo_path=hub_repo,
      data_processed_subpath=data_processed)
  
  #filter (2 week ahead horizon)
  component_forecast_2wk <-   component_forecast %>% 
    filter(horizon == "2" & type == "quantile")
  
  # extract log pdf and cdf values for training set forecasts
  # we add a little noise to the value column so that there is a density to
  # work with in case the forecaster had a point mass anywhere
  train_forecasts <- component_forecast_2wk %>%
    dplyr::mutate(
      value = rnorm(n = nrow(component_forecast_2wk), mean = value, sd = 0.1)
    ) %>%
    dplyr::inner_join(weekly_data_recent,
                      by = c("forecast_date" = "date", "location_name")) %>%
    dplyr::group_by(model, forecast_date, location_name, horizon, 
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
  
  #calculate percentages, correcting for negative numbers
  exp_forecast <- train_forecasts  %>%
    ungroup()  %>%
    dplyr::inner_join(weekly_data_recent,
                      by = c("forecast_date" = "date", "location_name"))  %>%
    mutate(large_increase=1-cdf_crit1,
           increase=cdf_crit1-cdf_crit2,
           stable=case_when(crit3>0  ~ cdf_crit2-cdf_crit3,
                            crit3<0 ~ cdf_crit2),
           decrease=case_when(crit3>0 & crit4 > 0 ~ cdf_crit3-cdf_crit4,
                              crit3>0 & crit4 < 0 ~ cdf_crit3),
           large_decrease=case_when(crit4>0 ~ cdf_crit4)) %>%
    select(forecast_date,location,location_name, large_increase,increase,stable,decrease,large_decrease)
  
  #transpose data_frame to format for submission
  exp_t = melt(exp_forecast,id.vars = c("forecast_date","location","location_name"),measure.vars = c("large_increase","increase","stable","decrease","large_decrease") , 
               variable.name="type_id", value.name="value",na.rm = TRUE) 
  exp_t <-exp_t %>%
    mutate(target="2 wk flu hosp rate change",
           type="category")  %>%
    select(forecast_date, target, location, location_name, type, type_id, value)
  
  #########################
  # output submission data
  #########################
  
  submission <-paste0(output1, models,"/",this_monday,"-",models,".csv")
  submission1 <-paste0(output2,this_monday,"-",models,".csv")
  write.csv(exp_t,submission,row.names=FALSE)
  if (!is.null(output2)) write.csv(exp_t,submission1,row.names=FALSE)
  
  #######################################################
  # Plot original forecasts and Experimental forecasts
  ####################################################### 
  
  #specify path to save PDF to
  destination <-paste0(output3,this_monday,"-",models,".pdf")
  #open PDF
  pdf(file=destination,paper='a4r')
  # pdf(file=destination)
  for (i in 1:54) {
    p1<-plot_forecasts(forecast_data=component_forecast, 
                       hub = 'FluSight',
                       truth_data=weekly_data_all,
                       # location = the_locations[i],
                       location = the_locations[i],
                       target_variable="inc flu hosp",
                       models=models,
                       # facet = ~model, facet_scales = "fixed", title = "default",
                       show_caption = TRUE,
                       truth_source="HealthData",
                       title='Original forecast',
                       plot=FALSE) +
      theme(legend.position = c(.05,.95), legend.justification = c(0,1), legend.key = element_rect(colour = "transparent", fill = "white"), 
            legend.background = element_rect(alpha("white", 0.5)), legend.box="horizontal")
    
    exp_t_1 <-exp_t  %>%
      filter(location== the_locations[i])
    
    p2 <- ggplot(exp_t_1, aes(fill=type_id, y=value, x=location_name)) +
      geom_bar(position="stack", stat="identity") +
      labs(title='Experimental target') 
    
    grid.arrange(p1,p2,nrow=1, widths = c(2,1))
  }
  dev.off()
  return (exp_t)
}

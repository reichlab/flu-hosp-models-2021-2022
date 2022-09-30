#' Fit an ARIMA model for Massachusetts, using FABLE
#' Transformation: Cube root, nonseasonal
#' Historic data: Syndromic data from MDPH 2017-2021 (request access of data from Nick Reich, Evan Ray or Martha Zorn 
#'                                               stored on OneDrive - University of Massachusetts\BoxData\Encrypted\Reichlab\model influenza hospitalizations)
#'                 Healthdata October 1, 2021-present
#'
#'Load packages

library(DT)
library(readxl)
library(tidyverse)
library(covidHubUtils)
library (MMWRweek)
library(fable)
library(fpp3)
library(glancedata)
library(gridExtra)

library(lubridate)
library(scico)
library(htmltools)
library (zoo)

#' Get load_flu_hosp_data function
setwd("~/GitHub/flu-hosp-models-2021-2022/R")
source("load_flu_hosp_data.R")

#' Load syndromic data 
setwd("~/GitHub/flu-hosp-models-2021-2022")
maflu_hosp <- read_excel("data/MDPH Syndromic Data_Weekly Influenza Admissions_UMASS Forecasting_15Dec2021.xlsx", sheet = "Data")
pastf <- maflu_hosp %>% 
  filter(! is.na(`MMWR Week`)) %>% 
  mutate(tot_count = as.numeric(`Total Admission Count`),
  flu_count = as.numeric(`Influenza Associated Admission Count`),
  MMWRyear = as.character(substr(`MMWR Week`,1,4)),
  MMWRweek = as.character(substr(`MMWR Week`,6,7)),
  year = as.numeric(substr(`MMWR Week`,1,4)),
  week = as.numeric(substr(`MMWR Week`,6,7)),
  mmwr_date = MMWRweek2Date(MMWRyear=year,MMWRweek=week,MMWRday=NULL)+5 #end date of week
)   %>%
  filter(mmwr_date <= as.Date("2021-09-30"))  %>%
  select(mmwr_date, flu_count)
  

#' read in Healthdata
#' 
#' @param locations optional list of FIPS or location abbreviations. Defaults to
#'   the US, the 50 states, DC, PR, and VI.
#' @param temporal_resolution "daily" or "weekly"
currentf <- load_flu_hosp_data(location = "25", temporal_resolution = "weekly") %>% 
  mutate(mmwr_date = lubridate::as_date(date),
         flu_count = value) %>%
  filter(location_name == "Massachusetts") %>%
  filter(mmwr_date > as.Date("2021-09-30")) %>%
  select(mmwr_date, flu_count) 

#' combine and set data to time series tsibble
flu_all <- pastf %>%
  rbind(currentf) 

flu_allf <- as_tibble(flu_all) %>%
  mutate(week=yearweek(mmwr_date))  %>%
   as_tsibble(index = week)

#' Define cube root transformation
cube_rt<- function(x) {x^(1/3)}
truncated_inv_cube_rt <- function(x){
  pmax(0,x)^3
}
cube_rt_transformation <- new_transformation(cube_rt, truncated_inv_cube_rt)

#' create forecast
  fit_cb<-flu_allf %>%
    model(
      ARIMA_cube_root = (ARIMA(cube_rt_transformation(flu_count) ~1 + pdq() + PDQ(0,0,0)))) 
  fit_cb %>% pivot_longer(everything(), names_to = "Model name",
                          values_to = "Orders")   
  
  fit_cb_f<-fit_cb %>% 
    forecast(h = 4,point_forecast = list(.mean = mean, .median = median)) 


#'  Read to pdf  
  #specify path to save PDF to
  destination = 'UMASS MA flu hosp (cube root) nonseasonal.pdf'
  pdf(destination)
  
  #plot
  title<-  paste0("MA ARIMA flu", Sys.Date())
  recent <- flu_allf %>%
    filter_index("2021 W39" ~.)

  fit_cb_f%>%
    autoplot(recent) +
    labs(title=title)
  
  #turn off PDF plotting
  dev.off() 
  
  #' Reformat hospitalization forecast to submission file
  #' 
  #' Note: target is "x weeks ahead inc hospitalization" and location is "25". 
  #' forecast values are rounded down to integers. 
  #' 
  #' @param forecast data.frame from `forecast(..., point_forecast = list(.mean = mean, .median = median))`
  #' @param forecast_date string of date
  #' 
  #' @return data.frame with columns target_end_date, value, type, quantile
  #' forecast_date, target and location
  to_submission_df <- function(forecast, forecast_date,truth){
    interval_level <-c(10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 98)
    
    #' last data of truth
    last_date <- dplyr::last(flu_allf$mmwr_date) 
    
    
    #' create quantile forecast
    forecast<- fit_cb_f %>%
      hilo(level = interval_level) %>%
      unpack_hilo(!! paste0(interval_level,"%")) %>%
      dplyr::select(-flu_count, -.model)   %>%
      mutate(horizon=row_number(),
             target_end_date = case_when(horizon==1 ~ last_date+7,
                                         horizon==2 ~ last_date+14,
                                         horizon==3 ~ last_date+21,
                                         horizon==4 ~ last_date+28))
    
    
    #' reformat quantile forecast
    suppressWarnings(quantile_forecast <- forecast %>%
                       select(-.mean) %>%
                       tidyr::pivot_longer(
                         cols = contains(c("%",".median")),
                         names_to = "quantile",
                         values_to = "value"
                       ) %>%
                       # calculate quantile levels from prediction interval levels
                       tidyr::separate(quantile, into = c("PI_level","bounds"), sep = "%_")  %>%
                       dplyr::mutate(PI_level = ifelse(PI_level == ".median", 0.5, as.numeric(PI_level)),
                                     type = "quantile")  %>%
                       dplyr::mutate(quantile = case_when(
                         bounds == "lower" ~ format((100-PI_level)/200, digits=3, nsmall=3),
                         bounds == "upper" ~ format((100-(100-PI_level)/2)/100, digits=3, nsmall=3),
                         is.na(bounds) ~ format(0.5, digits=3, nsmall=3))) %>%
                       dplyr::select(-bounds, -PI_level))
    
    quantile_forecast<-as.data.frame(quantile_forecast)  %>%
      select(-week)
    print(quantile_forecast)
    
    #' reformat point forecast
    point_forecast <- forecast %>%
      select(.mean, target_end_date,horizon,-week) %>%
      dplyr::mutate(type = "point") %>%
      dplyr::rename(value = .mean)
    
    point_forecast<-as.data.frame(point_forecast)  %>%
      select(-week)
    print(point_forecast)
    
    #' bind two data frames and add additional columns
    submission_df <- merge(quantile_forecast, point_forecast,all = TRUE) %>%
      dplyr::mutate(forecast_date = as.Date (forecast_date),
                    target = paste(as.numeric(horizon), "wk ahead inc flu hosp"),
                    location = "25",
                    value = floor(value)) %>%
      select(-horizon)
    
    
    return(submission_df)
  }
  
#' return forecast submission file
  submit<-to_submission_df(forecast=fit_cb_f, forecast_date=Sys.Date(),truth=flu_allf)
  file_name <- paste0(format(Sys.Date()),"-Umass-ARIMA.csv")
  write.csv(submit,file_name,row.names=FALSE)
  
  
  
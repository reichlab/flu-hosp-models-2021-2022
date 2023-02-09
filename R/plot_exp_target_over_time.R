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
library(lubridate)

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
      select(location,location_name, count_rate1per100k, count_rate2per100k)
    
    # load truth and merge with location data
    #    filter most recent truth
    #    set rate, diff, criteria
    weekly_data_all <- load_truth(hub = 'FluSight',temporal_resolution = "weekly") %>% 
      filter(target_end_date >= prior_10wk_eval_sat) %>%
      select(-location) %>%
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
      
      dplyr::select(model, location_name, location, value, target_end_date, target_variable, population,count_rate1per100k, count_rate2per100k, crit1, crit2, crit3, crit4)  %>%
      dplyr::filter(!is.na(value)) 
      
      
      # 2 week criteria
      p2week_crit <- weekly_data_all %>% 
        mutate(forecast_date=target_end_date+2, target_end_date=target_end_date+14) %>%  
        dplyr::select(model,  location, crit1, crit2, crit3, crit4, target_end_date, forecast_date)   
      

      # find 2 week post truth
      p2week_truth <- weekly_data_all  %>% 
        select(model, location_name, location, value, target_end_date, target_variable) %>%
        dplyr::inner_join(p2week_crit ,
                          by = c("model","location","target_end_date")) %>%
      mutate(large_increase=case_when(value>=crit1 ~ '*',
                                      value<crit1 ~ ' '),
             increase=case_when(value<crit1 & value >= crit2 ~ '*',
                                value<crit2 | value >=crit1 ~ ' '),
             stable=case_when(value<crit2 & value >= crit3 ~ '*',
                              value<crit3 | value >=crit2~ ' '),
             decrease=case_when(value<crit3 & value >= crit4 ~ '*',
                                value<crit4 | value >= crit3 ~ ' '),
             large_decrease=case_when(value<crit4 ~ '*',
                                      value>crit4 ~ ' ')) %>% 
      
      dplyr::select(model, location_name, location, value, forecast_date, target_variable, large_increase, increase, stable, decrease, large_decrease) %>%
      dplyr::filter(!is.na(value)) %>% 
        rename("forecast_date_x"="forecast_date")
    
      #transpose data_frame 
      p2week_t = melt(p2week_truth,id.vars = c("forecast_date_x","location","location_name"),measure.vars = c("large_increase","increase","stable","decrease","large_decrease") , 
                   variable.name="type_id", value.name="lvalue",na.rm = TRUE) 

    # list of locations
    the_locations <- location_data %>%  pull(location) #states, us and territories
    the_locations_name <- location_data %>%  pull(location_name) #states, us and territories 
    
    # rate cutoff
    one_100k <- location_data %>%  pull(count_rate1per100k) 
    two_100k <- location_data %>%  pull(count_rate2per100k) 
    
    # Load past forecasts
    setwd("~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental")   
    # find number of weeks to pull forecasts
    n_weeks = difftime(this_monday,"2022-12-12", units="weeks")
    n_weeks1=as.numeric(round(n_weeks))+1
    
    #create empty data frame for forecasts
    forecasts = data.frame(forecast_date=character(),
                            target=character(),
                            location=character(),
                            type=character(),
                            type_id=character(),
                            value=numeric())
     
    # Read in forecasts
    for(i in 1:n_weeks1) {
      new_date <- this_monday-(i-1)*7
      df_dates<-paste0(new_date, "-UMass-trends_ensemble.csv")
      n_dates<-paste0(new_date, "-UMass-trends_ensemble")
      z<-read.csv(df_dates)  %>% 
        select(forecast_date, target, location, type, type_id, value)
      forecasts=rbind(forecasts,z)
    }
    
    # add label for experimental target truth
    forecasts_t<-forecasts %>%
      mutate(forecast_date_x=as.Date(forecast_date)) %>% 
      dplyr::left_join(p2week_t ,
                        by = c("forecast_date_x","location","type_id")) %>%
      mutate(lvalue = ifelse(is.na(lvalue),' ',lvalue))
    
    
    #######################################################
    # Plot original forecasts and Experimental forecasts
    ####################################################### 
    
    #specify path to save PDF to
    destination <-paste0("~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/plots/",this_monday,"-2023 season.pdf")
    #open PDF
    pdf(file=destination,paper='a4r')
    # pdf(file=destination)
    for (i in 1:54) {
      
      weekly<-weekly_data_all %>%
        filter(location==the_locations[i])
      
      p1<-ggplot(data = weekly, aes(x = target_end_date, y = value)) +
        #geom_line(color = "black") +
        geom_point() +
        geom_line(color = "black") +
        scale_x_date(name=NULL, date_breaks="1 week", date_labels = "%Y %m %d") +
        theme(axis.text.x=element_text(angle=60,hjust=1)) +
        ylab("Incident Hospitalizations")+
             labs(title = paste("Truth data for ", the_locations_name[i]),
             subtitle = paste("Rates:\n  1/100k=",one_100k[i],"\n  2/100k=",two_100k[i]),
             caption="source: HealthData")+
        theme(legend.position = c(.05,.95), legend.justification = c(0,1),panel.grid.major=element_line(colour="black"))  
      
      casts <-forecasts_t %>%
        filter(location== the_locations[i])
      casts$type_id <- factor(casts$type_id, levels=c("large_increase","increase","stable","decrease","large_decrease"))
      
      p2 <- ggplot(casts, aes(fill=type_id, y=value, x=forecast_date,)) +
        geom_bar(position="stack", stat="identity") +
        theme(axis.text.x=element_text(angle=60,hjust=1)) +
        geom_text(aes(label=paste0(lvalue)),
                  position=position_fill(vjust=0.5),size=4, colour="white") +
        labs(title='Experimental target: \n  UMass-trends_ensemble',
            subtitle='Target for 2 weeks post forecast date \n (* indicates truth)') 
      
      grid.arrange(p1,p2,nrow=1, widths = c(4,5))
    }
    dev.off()


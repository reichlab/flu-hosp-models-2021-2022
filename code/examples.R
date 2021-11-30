# examples of how to use load_fluHosp_truth() function
source("./R/load_flu_hosp_data.R")

# default options for all arguments 
fluHosp_default<- load_flu_hosp_data(temporal_resolution="weekly")
fluHosp_default_daily<- load_flu_hosp_data(temporal_resolution="daily")
# the result for 3 states 
fluHosp1<- load_flu_hosp_data(locations=c("MA","CA","MD"),temporal_resolution="weekly")


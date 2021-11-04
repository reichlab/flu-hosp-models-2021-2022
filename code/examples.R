# examples of how to use load_fluHosp_truth() function
source("./R/load_fluHosp_truth.R")
# default will build a dataframe with 1 row for each state for the most recent Monday (and the most recent issue date)
fluHosp_default<- load_fluHosp_truth()
# this is the result for MA with a specified truth date and a default issue date (the most recent issue date in epidata)
fluHosp1<- load_fluHosp_truth(locations="MA",truth_date="2021-11-01")
# the result for MA with two specified truth dates
fluHosp2<- load_fluHosp_truth(locations="MA",truth_date=c("2021-11-01","2021-05-29"))
# the result for 3 states for one date
fluHosp3<- load_fluHosp_truth(locations=c("MA","CA","MD"),truth_date=c("2021-05-29"))
# the result for 3 states for two dates
fluHosp4<- load_fluHosp_truth(locations=c("MA","CA","MD"),truth_date=c("2021-05-29","2021-11-01"))



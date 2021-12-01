# examples of how to use load_fluHosp_truth() function
library(ggplot2)
source("./R/load_flu_hosp_data.R")

# default options for all arguments
flu_hosp_default <- load_flu_hosp_data()
ggplot(data = flu_hosp_default) +
  geom_line(mapping = aes(x = date, y = value)) +
  facet_wrap(vars(location_name), scales = "free_y")

# weekly data
flu_hosp_weekly <- load_flu_hosp_data(temporal_resolution = "weekly")
ggplot(data = flu_hosp_weekly) +
  geom_line(mapping = aes(x = date, y = value)) +
  facet_wrap(vars(location_name), scales = "free_y")

# the result for 3 states
flu_hosp1 <- load_flu_hosp_data(locations = c("MA", "CA", "MD"),
                                temporal_resolution = "weekly")
ggplot(data = flu_hosp1) +
  geom_line(mapping = aes(x = date, y = value)) +
  facet_wrap(vars(location_name), scales = "free_y")

# result for US -- generates an error
flu_hosp_us <- load_flu_hosp_data(locations = "US")
ggplot(data = flu_hosp_us) +
  geom_line(mapping = aes(x = date, y = value)) +
  facet_wrap(vars(location_name), scales = "free_y")

# result for US and MA -- no error, but incorrect data for US
flu_hosp_us_ma <- load_flu_hosp_data(locations = c("MA", "US"))
ggplot(data = flu_hosp_us_ma) +
  geom_line(mapping = aes(x = date, y = value)) +
  facet_wrap(vars(location_name), scales = "free_y")

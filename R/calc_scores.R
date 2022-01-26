library(covidHubUtils)
library(ggplot2)
fcasts <- load_forecasts(source = "local_hub_repo",dates=c("2022-01-17"),targets = c(paste0(rep(1:4)," wk ahead inc flu hosp")),
                         data_processed_subpath = "data-forecasts/" ,hub_repo_path = "../Flusight-forecast-data/",hub="FluSight" )

truth <- load_truth(hub = "FluSight")

scores <- score_forecasts(fcasts,truth = truth,metrics = c("abs_error","wis"),use_median_as_point=TRUE)

ggplot(scores,aes(x=model,y=log(wis))) + geom_boxplot() + theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_hline(yintercept = median(log(scores[scores$model == "Flusight-baseline",]$wis))) + facet_wrap(~ horizon)

library(dplyr)

scores %>% group_by(model) %>% summarize(wis=median(wis))  %>% arrange(wis) %>% as.data.frame()

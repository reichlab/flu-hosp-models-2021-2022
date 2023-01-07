source('./../../R/create_exp_target.R')

print(getwd())
library (usethis)

test_that("correct format does not produce error", {
  
  expect_no_error(suppressMessages(create_exp_target(models = "UMass-trends_ensemble",
                                                  source = "local_hub_repo", 
                                                  hub_repo="~/GitHub/flu-hosp-models-2021-2022",
                                                  data_processed="./weekly-submission/forecasts/",
                                                  c_target="2 wk flu hosp rate change",
                                                  output1="~/GitHub/Flusight-forecast-data/data-experimental/tests/",
                                                  output2="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/tests/",
                                                  output3="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/plots/tests/")))
})

test_that("incorrect model name should throw error", {
  
  expect_error(suppressMessages(create_exp_target(models = "source",
                                                  source = "local_hub_repo", 
                                                  hub_repo="~/GitHub/flu-hosp-models-2021-2022",
                                                  data_processed="./weekly-submission/forecasts/",
                                                  c_target="2 wk flu hosp rate change",
                                                  output1="~/GitHub/Flusight-forecast-data/data-experimental/tests/",
                                                  output2="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/tests/",
                                                  output3="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/plots/tests/")))
})

test_that("missing hub_repo should throw error", {

  expect_error(suppressMessages(create_exp_target(models = "UMass-trends_ensemble",
                                                  source = "local_hub_repo",
                                                  data_processed="./weekly-submission/forecasts/",
                                                  c_target="2 wk flu hosp rate change",
                                                  output1="~/GitHub/Flusight-forecast-data/data-experimental/tests/",
                                                  output2="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/tests/",
                                                  output3="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/plots/tests/")))
})

test_that("incorrect hub_repo should throw error", {
  
  expect_error(suppressMessages(create_exp_target(models = "UMass-trends_ensemble",
                                                  source = "local_hub_repo",
                                                  hub_repo="xxx",
                                                  data_processed="./weekly-submission/forecasts/",
                                                  c_target="2 wk flu hosp rate change",
                                                  output1="~/GitHub/Flusight-forecast-data/data-experimental/tests/",
                                                  output2="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/tests/",
                                                  output3="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/plots/tests/")))
})


test_that("missing data_processed should throw error", {
  
  expect_error(suppressMessages(create_exp_target(models = "UMass-trends_ensemble",
                                                  source = "local_hub_repo", 
                                                  hub_repo="~/GitHub/flu-hosp-models-2021-2022",
                                                  c_target="2 wk flu hosp rate change",
                                                  output1="~/GitHub/Flusight-forecast-data/data-experimental/tests/",
                                                  output2="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/tests/",
                                                  output3="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/plots/tests/")))
})

test_that("incorrect data_processed should throw error", {
  
  expect_error(suppressMessages(create_exp_target(models = "UMass-trends_ensemble",
                                                  source = "local_hub_repo", 
                                                  hub_repo="~/GitHub/flu-hosp-models-2021-2022",
                                                  data_processed="xxx",
                                                  c_target="2 wk flu hosp rate change",
                                                  output1="~/GitHub/Flusight-forecast-data/data-experimental/tests/",
                                                  output2="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/tests/",
                                                  output3="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/plots/tests/")))
})

test_that("missing c_target should throw error", {
  
  expect_error(suppressMessages(create_exp_target(models = "UMass-trends_ensemble",
                                                  source = "local_hub_repo", 
                                                  data_processed="./weekly-submission/forecasts/",
                                                  hub_repo="~/GitHub/flu-hosp-models-2021-2022",
                                                  output1="~/GitHub/Flusight-forecast-data/data-experimental/tests/",
                                                  output2="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/tests/",
                                                  output3="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/plots/tests/")))
})

test_that("missing output1 should throw error", {
  
  expect_error(suppressMessages(create_exp_target(models = "UMass-trends_ensemble",
                                                  source = "local_hub_repo", 
                                                  data_processed="./weekly-submission/forecasts/",
                                                  hub_repo="~/GitHub/flu-hosp-models-2021-2022",
                                                  c_target="2 wk flu hosp rate change",
                                                  output2="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/tests/",
                                                  output3="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/plots/tests/")))
})

test_that("incorrect output1 should throw error", {
  
  expect_error(suppressMessages(create_exp_target(models = "UMass-trends_ensemble",
                                                  source = "local_hub_repo", 
                                                  data_processed="./weekly-submission/forecasts/",
                                                  hub_repo="~/GitHub/flu-hosp-models-2021-2022",
                                                  c_target="2 wk flu hosp rate change",
                                                  output1="xxx",
                                                  output2="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/tests/",
                                                  output3="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/plots/tests/")))
})

test_that("missing output2 should NOT throw error", {
  
  expect_no_error(suppressMessages(create_exp_target(models = "UMass-trends_ensemble",
                                                  source = "local_hub_repo", 
                                                  data_processed="./weekly-submission/forecasts/",
                                                  hub_repo="~/GitHub/flu-hosp-models-2021-2022",
                                                  c_target="2 wk flu hosp rate change",
                                                  output3="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/plots/tests/")))
})

test_that("incorrect output2 should throw error", {
  
  expect_error(suppressMessages(create_exp_target(models = "UMass-trends_ensemble",
                                                  source = "local_hub_repo", 
                                                  data_processed="./weekly-submission/forecasts/",
                                                  hub_repo="~/GitHub/flu-hosp-models-2021-2022",
                                                  c_target="2 wk flu hosp rate change",
                                                  output1="~/GitHub/Flusight-forecast-data/data-experimental/tests/",
                                                  output2="xxx",
                                                  output3="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/plots/tests/")))
})

test_that("missing output3 should throw error", {
  
  expect_error(suppressMessages(create_exp_target(models = "UMass-trends_ensemble",
                                                  source = "local_hub_repo", 
                                                  data_processed="./weekly-submission/forecasts/",
                                                  hub_repo="~/GitHub/flu-hosp-models-2021-2022",
                                                  c_target="2 wk flu hosp rate change",
                                                  output1="~/GitHub/Flusight-forecast-data/data-experimental/tests/",
                                                  output2="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/tests/")))
})

test_that("incorrect output3 should throw error", {
  
  expect_error(suppressMessages(create_exp_target(models = "UMass-trends_ensemble",
                                                  source = "local_hub_repo", 
                                                  data_processed="./weekly-submission/forecasts/",
                                                  hub_repo="~/GitHub/flu-hosp-models-2021-2022",
                                                  c_target="2 wk flu hosp rate change",
                                                  output1="~/GitHub/Flusight-forecast-data/data-experimental/tests/",
                                                  output2="~/GitHub/flu-hosp-models-2021-2022/weekly-submission/forecasts/data-experimental/tests/",
                                                  output3="xxx")))
})
context("plot")

library(NPS)
library(NPSdrivers)
library(dplyr)

data(survey_sim)

NPS <- NPS::nps(survey_sim$recommend)
survey_sim <- survey_sim %>%
  mutate(NPC = factor(npc(recommend), ordered = TRUE)) %>%
  select(-recommend)

m <- model_survey(survey_sim)
scenario_results <- run_scenarios(m)


test_that("plot.NPSdriverScenarios() works", {

  p <- try(plot(scenario_results, NPS))

  expect_true("ggplot" %in% class(p))

})

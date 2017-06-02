context("run_scenarios")

library(NPS)
library(NPSdrivers)
library(dplyr)

data(survey_sim)

survey_sim <- survey_sim %>%
  mutate(NPC = factor(npc(recommend), ordered = TRUE)) %>%
  select(-recommend)

m <- model_survey(survey_sim)
scenario_results <- run_scenarios(m)

terms <- attr(m$terms, "term.labels")

test_that("run_scenarios() works", {

  expect_identical(c("scenario", "description", "expected_NPS", "opportunity"),
                   names(scenario_results))
  expect_true(dim(scenario_results)[1] == 2*length(terms) + dim(combn(terms, 2))[2])

})

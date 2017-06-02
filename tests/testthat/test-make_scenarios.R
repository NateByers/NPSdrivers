context("make_scenarios")

data("survey_sim")

survey_sim <- survey_sim %>%
  mutate(NPC = factor(NPS::npc(recommend), ordered = TRUE)) %>%
  select(-recommend)

m <- model_survey(survey_sim)

scenarios <- make_scenarios(m)

terms <- attr(m$terms, "term.labels")

test_that("make_scenarios() output is correct", {

  expect_identical(c("scenario", "description", "variable", "current_value",
                     "changed_value"), names(scenarios))
  expect_true(length(unique(scenarios$scenario)) == 2*length(terms) + dim(combn(terms, 2))[2])

})

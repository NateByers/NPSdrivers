context("tidy")

data("survey_sim")

data <- survey_sim %>%
  mutate(NPC = factor(NPS::npc(recommend), ordered = TRUE)) %>%
  select(-recommend)

m <- model_survey(data)

tidy_output <- tidy(m)

test_that("tidy() works", {

  expect_identical(c("Variable", "Parameter", "Value", "Std. Error", "t value",
                     "p value", "Odds Ratio", "2.5 %", "97.5 %"),
                   names(tidy_output))
  expect_true(length(c(m$coefficients, m$zeta)) == dim(tidy_output)[1])

})

augment_output <- augment(m)

test_that("augment() works", {

  expect_identical(c(names(m$model), ".fitted", ".Detractor", ".Passive",
                     ".Promoter"),
                   names(augment_output))
  expect_true(dim(survey_sim)[1] == dim(augment_output)[1])

})

glance_output <- glance(m)

test_that("glance() works", {

  expect_identical(c("residual.deviance", "df.residual", "AIC"),
                   names(glance_output))
  expect_true(dim(glance_output)[1] == 1)


})

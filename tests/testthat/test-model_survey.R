context("model_survey")

data("survey_sim")

test_that("you get an error if you don't have an NPC column", {

            expect_error(
              model_survey(survey_sim)
            )

          })

data <- survey_sim %>%
  mutate(NPC = factor(NPS::npc(recommend), ordered = TRUE)) %>%
  select(-recommend)

m <- model_survey(data)

test_that("you get an object with the class polr", {

  expect_identical(class(m), c("NPSdrivers", "polr"))

})

test_that("you only get independent variables that are correlated with NPC", {

  expect_true(sum(!grepl("^corr", attr(m$terms, "term.labels"))) == 0)

})



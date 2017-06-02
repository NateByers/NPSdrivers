context("confusion_matrix")

library(caret)

data("survey_sim")

survey_sim <- survey_sim %>%
  mutate(NPC = factor(NPS::npc(recommend), ordered = TRUE)) %>%
  select(-recommend)

index <- caret::createDataPartition(survey_sim$NPC, p = .8, list = FALSE, times = 1)
train_df <- survey_sim[index, ]
test_df <- survey_sim[-index, ]

m <- model_survey(train_df)

tble <- confusion_matrix(m, test_df)

test_that("confusion_matrix() works", {

  expect_true(class(tble) == "table")
  expect_identical(attr(tble, "dimnames"),
                   list(prediction = c("Detractor", "Passive", "Promoter"),
                        c("Detractor", "Passive", "Promoter")))

})

accurate <- accuracy(m, test_df)

test_that("accuracy() works", {

  expect_true(class(accurate) == "numeric")
  expect_true(accurate <= 1)
  expect_true(accurate >= 0)

})

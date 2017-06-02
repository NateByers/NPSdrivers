#' Calculate confusion matrix and accuracy for Net Promoter Category
#' predicitons
#' @export
#' @param m Model output from \code{model_survey}
#' @param test_data A data frame with test data in the same format as the input
#' data for the model object
#' @examples
#' library(dplyr)
#' library(NPS)
#' library(caret)
#'
#' data(survey_sim)
#'
#' survey_sim <- survey_sim %>%
#'   mutate(NPC = factor(npc(recommend), ordered = TRUE)) %>%
#'   select(-recommend)
#'
#' index <- createDataPartition(survey_sim$NPC, p = .8, list = FALSE, times = 1)
#' train <- survey_sim[index, ]
#' test <- survey_sim[-index, ]
#'
#' m <- model_survey(train)
#'
#' confusion_matrix(m, test)
#' accuracy(m, test)
confusion_matrix <- function(m, test_data) {
  prediction <- predict(m, test_data)
  table(prediction, test_data$NPC)
}

#' @rdname confusion_matrix
#' @export
accuracy <- function(m, test_data) {
  prediction <- predict(m, test_data)
  sum(as.character(prediction) == as.character(test_data$NPC))/length(prediction)
}

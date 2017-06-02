#' Fit ordered logistic regression model to survey data
#'
#' @import dplyr
#' @importFrom MASS polr stepAIC
#' @export
#' @param data A data frame of survey results with an "NPC" column (for Net Promoter
#' Catagory). See example.
#' @param verbose Logical indicating if you want to see output from \code{stepAIC()}
#' @examples
#' library(NPS)
#' library(dplyr)
#'
#' data(survey_sim)
#'
#' survey_sim <- survey_sim %>%
#'   mutate(NPC = factor(npc(recommend), ordered = TRUE)) %>%
#'   select(-recommend)
#'
#' m <- model_survey(survey_sim)
model_survey <- function(data, verbose = FALSE) {

  if(!"NPC" %in% names(data) | !"factor" %in% class(data$NPC) |
     !identical(c("Detractor", "Passive", "Promoter"), levels(data$NPC))) {
    stop("there must be an NPC column created by the NPS::npc() function")
  }

  if(class(data$NPC)[1] == "factor") {
    data$NPC <- factor(data$NPC, ordered = TRUE)
  }

  indep_vars <- names(data)[!names(data) %in% "NPC"]

  m <- MASS::polr(NPC ~ 1, data = data, Hess = TRUE)
  upper_formula <- paste("~", paste(indep_vars, collapse = " + "))
  m_step <- MASS::stepAIC(m, scope = list(upper = formula(upper_formula),
                                          lower = ~ 1), trace = verbose)

  attr(m_step, "class") <- c("NPSdrivers", "polr")
  m_step
}

#' Tidy up the output from \code{model_survey()}
#'
#' @import broom
#' @param m Model output from \code{model_survey}
#' @export
#' @examples
#' library(NPS)
#' library(dplyr)
#' library(broom)
#'
#' data(survey_sim)
#'
#' survey_sim <- survey_sim %>%
#'   mutate(NPC = factor(npc(recommend), ordered = TRUE)) %>%
#'   select(-recommend)
#'
#' m <- model_survey(survey_sim)
#'
#' tidy(m)
#' head(augment(m))
#' glance(m)
tidy.NPSdrivers <- function(m) {
  coef_table <- coef(summary(m))

  coef_table <- coef_table %>%
    as.data.frame() %>%
    mutate(Parameter = row.names(coef_table),
           `p value` = pnorm(abs(`t value`), lower.tail = FALSE) * 2)

  OR <- exp(cbind("Odds Ratio" = coef(m), confint(m)))

  OR <- OR %>%
    as.data.frame() %>%
    mutate(Parameter = row.names(OR))

  coef_table <- left_join(coef_table, OR, "Parameter") %>%
    mutate(Variable = NA) %>%
    select(Variable, Parameter, Value, `Std. Error`, `t value`, `p value`,
           `Odds Ratio`, `2.5 %`, `97.5 %`)

  variables <- names(m$model)[names(m$model) != "NPC"]
  variables <- setNames(variables, variables)
  variables <- c("Detractor|Passive" = "NPC", "Passive|Promoter" = "NPC", variables)

  coef_table$Variable <- sapply(coef_table$Parameter, function(x) {
    variables[sapply(names(variables), function(y, x) {
      grepl(y, x, fixed = TRUE)
    }, x = x)]
  })

  coef_table
}

#' @rdname tidy.NPSdrivers
#' @export
augment.NPSdrivers <- function(m) {
  fit <- fitted(m) %>%
    as.data.frame() %>%
    mutate(fitted = predict(m)) %>%
    select(fitted, Detractor, Passive, Promoter)
  names(fit) <- paste0(".", names(fit))

  cbind(m$model, fit)
}

#' @rdname tidy.NPSdrivers
#' @export
glance.NPSdrivers <- function(m) {
  data.frame(residual.deviance = m$deviance, df.residual = m$df.residual,
             AIC = m$anova[dim(m$anova)[1], "AIC"])
}

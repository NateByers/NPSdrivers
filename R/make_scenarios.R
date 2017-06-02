#' Make table of scenarios
#' @export
#' @param m Model output from \code{model_survey()}
#' @param redistribution Proportion of highest score distributed to lower scores
#' @examples
#' library(NPS)
#' library(NPSdrivers)
#' library(dplyr)
#'
#' data(survey_sim)
#'
#' survey_sim <- survey_sim %>%
#'   mutate(NPC = factor(npc(recommend), ordered = TRUE)) %>%
#'   select(-recommend)
#'
#' m <- model_survey(survey_sim)
#' scenarios <- make_scenarios(m)
make_scenarios <- function(m, redistribution = 0.5) {

  df <- m$model %>%
    select(-NPC)
  df <- df[, sapply(df, function(x) identical(class(x), c("ordered", "factor")))]

  levels_df <- data.frame()
  for(i in names(df)) {
    levels_df_ <- data.frame(variable = i, level = c(levels(df[[i]]), "N/A"),
                            stringsAsFactors = FALSE) %>%
      mutate(order = row_number())
    levels_df_[grepl("NA|N/A", levels_df_$level), "order"] <- 0
    levels_df <- rbind(levels_df, levels_df_)
  }

  scenarios <- levels_df
  scenarios$scenario <- group_indices(levels_df, variable)
  scenarios <- scenarios %>%
    group_by(scenario) %>%
    mutate(changed_value = level[max(order)]) %>%
    filter(order != max(order)) %>%
    mutate(current_value = level,
           description = paste("change all", variable, "to", changed_value)) %>%
    ungroup() %>%
    select(scenario, description, variable, current_value, changed_value)

  max_scenario <- max(scenarios$scenario)
  combinations <- combn(unique(scenarios$scenario), 2)

  for(j in 1:dim(combinations)[2]) {
    scenarios_ <- scenarios %>%
      filter(scenario %in% combinations[ ,j]) %>%
      mutate(scenario = max_scenario + j)
    variables <- unique(scenarios_[["variable"]])
    changed_values <- unique(scenarios_[["changed_value"]])
    if(length(changed_values) == 2) {
      changed_values <- paste(changed_values, collapse = " and ")
    }
    scenarios_ <- scenarios_ %>%
      mutate(description = paste("change all", variables[1], "and", variables[2],
                                 "to", changed_values))
    scenarios <- rbind(scenarios, scenarios_)
  }

  max_scenario <- max(scenarios$scenario)

  for(k in 1:length(unique(levels_df$variable))) {
    variable_ <- unique(levels_df$variable)[k]
    highest_value <- levels_df %>%
      filter(variable == variable_, order == max(order))

    scenarios_ <- data.frame(scenario = max_scenario + k,
                             description = paste("lose half of the",
                                                 highest_value$level,
                                                 "answers for", variable_),
                             variable = variable_,
                             current_value = highest_value$level,
                             changed_value = paste0("redistribute|",
                                                    redistribution),
                             stringsAsFactors = FALSE)
    scenarios <- rbind(scenarios, scenarios_)
  }

  as.data.frame(scenarios)
}


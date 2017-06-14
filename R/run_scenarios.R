#' Run different scenarios on the survey data based on the model output from
#' \code{model_survey()}
#' @importFrom tidyr spread
#' @export
#' @param model Model object from \code{model_survey()}
#' @param scenarios A data frame of scenarios
#' @param variables_column Should the table output have a variables column
#' @param descending Should the table be in descending order by expected_NPS
#'  (scenarios will be renumbered)
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
#' scenario_results <- run_scenarios(m)
run_scenarios <- function(model, scenarios = make_scenarios(model),
                          variables_column = FALSE, descending = TRUE) {

  scenario_table <- scenarios
  scenarios <- unique(scenarios$scenario)
  data <- model$model

  output <- data.frame()
  for(k in scenarios) {
    # k <- scenarios[4]
    scenario_table_ <- scenario_table %>%
      filter(scenario == k)

    opportunity <- get_opportunity(data, scenario_table_)

    if(grepl("^redistribute", scenario_table_$changed_value[1])) {
      type = "redistribute"
    } else {
      type = "simple"
    }
    data_ <- data %>%
      scenario_transform(scenario_table_, type = type)
    NPS <- cbind(data_, as.data.frame(predict(model, data_, type = "probs"))) %>%
      summarise(Detractor = mean(Detractor), Promoter = mean(Promoter),
                NPS = Promoter - Detractor)
    rm(data_)
    output <- rbind(output, data.frame(scenario = k, expected_NPS = NPS$NPS,
                                       opportunity = opportunity))
  }

  scenario_table_ <- scenario_table %>%
    filter(scenario %in% scenarios)

  if(variables_column) {
    scenario_table_ <- scenario_table %>%
      group_by(scenario) %>%
      summarize(variables = paste(unique(variable), collapse = " "),
                description = ifelse(grepl("change", unique(description)),
                                     "change all answers to highest score",
                                     "lose half of highest score answers")) %>%
      ungroup()
  } else {
    scenario_table_ <- scenario_table %>%
      dplyr::select(scenario, description) %>%
      distinct()
  }

  scenario_table_ <- scenario_table_ %>%
    left_join(output, "scenario")

  if(descending) {
    if(variables_column) {
      scenario_table_ <- scenario_table_ %>%
        group_by(scenario) %>%
        mutate(group = ifelse(length(strsplit(variables, " ")[[1]]) == 1 & grepl("change", description), 1, NA),
               group = ifelse(length(strsplit(variables, " ")[[1]]) == 2 & grepl("change", description), 2, group)) %>%
        ungroup()
    } else {
      scenario_table_ <- scenario_table_ %>%
        mutate(group = ifelse(!grepl("change.*and", description), 1, NA),
               group = ifelse(grepl("change.*and", description), 2, group))
    }
    scenario_table_ <- scenario_table_ %>%
      mutate(group = ifelse(grepl("lose", description), 3, group)) %>%
      arrange(group, desc(expected_NPS)) %>%
      mutate(scenario = row_number()) %>%
      select(-group)
  } else {
    scenario_table_ <- scenario_table_ %>%
      arrange(scenario)
  }


  scenario_table_ <- scenario_table_ %>%
    mutate(scenario = factor(scenario),
           expected_NPS = round(expected_NPS, 3))

  class(scenario_table_) <- c("NPSdriverScenarios", class(scenario_table_))

  scenario_table_
}

get_opportunity <- function(df, scenario_table_) {
  # df <- data
  df_ <- df %>%
    mutate(id = row_number())
  ids <- c()
  for(m in unique(scenario_table_$variable)) {
    # m = unique(scenario_table_$variable)[1]
    filter_df <- scenario_table_ %>%
      filter(variable == m)
    df_$current_value <- as.character(df_[[m]])
    df_ <- semi_join(df_, filter_df, c("current_value"))
    ids <- unique(c(ids, df_$id))
  }

  opportunity <- round(length(ids)/dim(df)[1], 2)

  if(grepl("redistribute", scenario_table_[["changed_value"]][1])) {
    opportunity <- opportunity/2
  }

  opportunity
}

scenario_transform <- function(df, scenario, type = c("simple", "redistribute")) {
  if(type == "simple") {
    for(i in unique(scenario$variable)) {
      # i <- unique(scenario$variable)[1]
      scenario_ <- scenario %>%
        filter(variable == i)
      for(j in 1:dim(scenario_)[1]) {
        df[[i]][df[[i]] == scenario_[j, "current_value"]] <- scenario_[j, "changed_value"]
      }
    }
  } else if(type == "redistribute") {
    proportion <- as.numeric(strsplit(scenario$changed_value, "\\|")[[1]][2])
    current_value_l <- df[[scenario$variable]] == scenario$current_value
    current_value_l[current_value_l] <- sample(c(TRUE, FALSE),
                                               length(current_value_l[current_value_l]),
                                               replace = TRUE, prob = c(proportion, 1 - proportion))
    current_values <- df[[scenario$variable]][current_value_l]
    other_values <- levels(df[[scenario$variable]])
    other_values <- other_values[other_values != scenario$current_value]

    distribution <- df %>%
      filter_(paste0(scenario$variable, " != '", scenario$current_value, "'")) %>%
      group_by_(scenario$variable) %>%
      summarize(proportion = n()) %>%
      mutate(proportion = proportion/sum(proportion))

    other_values <- sample(distribution[[scenario$variable]],
                           length(current_values),
                           replace = TRUE,
                           prob = distribution$proportion)
    df[[scenario$variable]][current_value_l] <- other_values
  }

  df
}


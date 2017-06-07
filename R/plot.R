#' Plot NPS Driver Scenarios
#' @import ggplot2 graphics
#' @export
#' @param x Output from \code{run_scenarios()}
#' @param NPS The current NPS in the data
#' @param scenarios Vector of scenarios to plot
#' @param ... Parameters passed on to \code{aes()}
#' @examples
#' library(NPS)
#' library(NPSdrivers)
#' library(dplyr)
#'
#' data(survey_sim)
#'
#' NPS <- NPS::nps(survey_sim$recommend)
#' survey_sim <- survey_sim %>%
#'   mutate(NPC = factor(npc(recommend), ordered = TRUE)) %>%
#'   select(-recommend)
#'
#' m <- model_survey(survey_sim)
#' scenario_results <- run_scenarios(m)
#' plot(scenario_results, NPS)
#' plot(scenario_results, NPS, c(1, 2, 5))
plot.NPSdriverScenarios <- function(x, NPS, scenarios, ...) {

  if(!missing(scenarios)) {
    x <- x[x$scenario %in% scenarios, ]
  }

  p <- ggplot(x, aes(color = opportunity, ...)) +
    geom_segment(aes(x = scenario, xend = scenario, y = NPS, yend = expected_NPS),
                 size = 15) +
    scale_color_gradient(labels = percent) +
    ylab("expected_NPS") +
    geom_hline(aes(yintercept = NPS, linetype = "current"), color = 'black') +
    scale_linetype_manual(name = 'NPS', values = c(1),
                          guide = guide_legend(override.aes = list(color = c('black'))))

  p
}

percent <- function(x) {
  paste0(x*100, "%")
}



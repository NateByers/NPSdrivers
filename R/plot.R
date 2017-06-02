#' Plot NPS Driver Scenarios
#' @import ggplot2
#' @export
#' @param scenarios Output from \code{run_scenarios()}
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
#' plot(scenario_results)
# setMethod(
#   signature(x = "NPSdriverScenarios"),
#   function(x)
#   {
#     p <- ggplot(scenarios, aes(color = opportunity)) +
#       geom_segment(aes(x = scenario, xend = scenario, y = NPS::nps(survey$recommend), yend = expected_NPS),
#                    size = 15) +
#       scale_color_gradient(labels = percent) +
#       ylab("expected_NPS") +
#       geom_hline(aes(yintercept = NPS::nps(survey$recommend), linetype = "current"),
#                  color = 'black') +
#       scale_linetype_manual(name = 'NPS', values = c(1),
#                             guide = guide_legend(override.aes = list(color = c('black')))) +
#       theme(axis.text.x=element_blank())
#
#     print(p)
#   }
# )
#


% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_scenarios.R
\name{run_scenarios}
\alias{run_scenarios}
\title{Run different scenarios on the survey data based on the model output from
\code{model_survey()}}
\usage{
run_scenarios(model, scenarios = make_scenarios(model),
  variables_column = FALSE, descending = TRUE)
}
\arguments{
\item{model}{Model object from \code{model_survey()}}

\item{scenarios}{A data frame of scenarios}

\item{variables_column}{Should the table output have a variables column}

\item{descending}{Should the table be in descending order by expected_NPS
(scenarios will be renumbered)}
}
\description{
Run different scenarios on the survey data based on the model output from
\code{model_survey()}
}
\examples{
library(NPS)
library(NPSdrivers)
library(dplyr)

data(survey_sim)

survey_sim <- survey_sim \%>\%
  mutate(NPC = factor(npc(recommend), ordered = TRUE)) \%>\%
  select(-recommend)

m <- model_survey(survey_sim)
scenario_results <- run_scenarios(m)
}

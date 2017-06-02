

marginal <- list(c(0.02263724, 0.03338993, 0.05319751, 0.06564799, 0.08488964,
                   0.13525750, 0.15959253, 0.20826259, 0.32314658, 0.47425014),
                 c(0.07697842, 0.11438849, 0.15899281, 0.24316547, 0.39568345,
                   0.44172662),
                 c(0.07725322, 0.11281422, 0.15328020, 0.21459227, 0.36296750,
                   0.41079093),
                 c(0.006932409, 0.020219526, 0.099942230, 0.263431542,
                   0.556325823))
GenOrd::corrcheck(marginal)
# select a feasible correlation matrix
Sigma <- matrix(c(1.00, 0.75, 0.65, 0.03,
                  0.75, 1.00, 0.25, 0.04,
                  0.65, 0.25, 1.00, -0.05,
                  0.03, 0.04, -0.05, 1.00),
                4, 4, byrow=TRUE)

set.seed(1)
survey_sim <- GenOrd::ordsample(1000, marginal, Sigma) %>%
  as.data.frame()
names(survey_sim) <- c("recommend", "corr1", "corr2", "uncorr")
make_ordered_factor <- function(x) {
  factor(x, ordered = TRUE)
}
survey_sim <- survey_sim %>%
  mutate(recommend = recommend - 1) %>%
  mutate_each(funs(make_ordered_factor), corr1, corr2, uncorr)

devtools::use_data(survey_sim, overwrite = TRUE)

# Fits the hierarchical Bayesian IV model in random_effects.stan.
# Slow, so run this once before rendering CSL.Rtex; analysis.R reads the fit.

library(cmdstanr)

source("processing.R")

# variables are scaled, so analysis.R unscales the coefficients
stan_data <-
  list(
    number_of_instruments = 1,
    number_of_endogenous_variables = 1,
    number_of_country_years = nrow(present_data),
    number_of_outcome_observations = nrow(indexed_price_data),
    number_of_controls = ncol(controls_frame),
    number_of_countries = nrow(indexed_countries),

    instruments = scale(indexed_country_years$log_gdp_per_capita),
    endogenous_variables = scale(indexed_country_years$life_expectancy),
    controls = scale(controls_frame),
    country_index_of_country_year =
      indexed_country_years$country_index,

    outcomes = as.vector(scale(indexed_price_data$log_price_of_life)),
    country_year_index_of_observation =
      indexed_price_data$country_year_index
  )

stan_model <- cmdstan_model("random_effects.stan")

stan_fit <- stan_model$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 1
)

stan_fit$save_object(file = output_path("stan_fit.RDS"))

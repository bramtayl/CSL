library(cmdstanr)

country_year_data <- get_present_data_for(
  with_missing_outcomes,
  c(
    "life_expectancy",
    "log_gdp_per_capita",
    "log_price_of_life",
    unique_controls
  )
) %>%
  arrange(country, year) %>%
  mutate(country_year_index = 1:n())

price_data <-
  filter_price_data(transformed_prices) %>%
  filter(!is.na(log_price_of_life)) %>%
  inner_join(
    country_year_data %>%
    select(country, year, country_year_index),
    by = c("country", "year")
  ) %>%
  arrange(country_year_index)

indexed_countries <-
  country_year_data %>%
  select(country) %>%
  distinct %>%
  arrange(country) %>%
  mutate(country_index = seq_len(n()))

indexed_years <-
  country_year_data %>%
  select(year) %>%
  distinct %>%
  arrange(year) %>%
  mutate(year_index = seq_len(n()))

indexed_country_years <-
  country_year_data %>%
  left_join(
    indexed_countries %>%
      select(country, country_index),
    by = "country"
  ) %>%
  left_join(
    indexed_years %>%
      select(year, year_index),
    by = "year"
  )

log_gdp_per_capita_mean <-
  mean(country_year_data$log_gdp_per_capita)

log_gdp_per_capita_standard_deviation <-
  sd(country_year_data$log_gdp_per_capita)

life_expectancy_mean <-
  mean(country_year_data$life_expectancy)

life_expectancy_standard_deviation <-
  sd(country_year_data$life_expectancy)

log_price_of_life_mean <-
  mean(price_data$log_price_of_life)

log_price_of_life_standard_deviation <-
  sd(price_data$log_price_of_life)

controls_frame <-
  country_year_data[significant_controls]

controls_means <-
  controls_frame %>%
  lapply(mean)

controls_standard_deviations <-
  controls_frame %>%
  lapply(sd)

stan_data <-
  list(
    number_of_country_years = nrow(country_year_data),
    number_of_outcome_observations = nrow(price_data),
    number_of_controls = length(significant_controls),
    number_of_countries = nrow(indexed_countries),
    number_of_years = nrow(indexed_years),

    instrument =
    (indexed_country_years$log_gdp_per_capita - log_gdp_per_capita_mean) /
    log_gdp_per_capita_standard_deviation,
    endogenous_variable =
    (indexed_country_years$life_expectancy - life_expectancy_mean) /
    life_expectancy_standard_deviation,
    controls =
    lapply(controls_frame, scale) %>%
    as_tibble %>%
    as.matrix,
    country_index_of_country_year =
    indexed_country_years$country_index,
    year_index_of_country_year =
    indexed_country_years$year_index,

    outcomes = (price_data$log_price_of_life - log_price_of_life_mean) /
    log_price_of_life_standard_deviation,
    country_year_index_of_observation =
    price_data$country_year_index
  )

stan_model <- cmdstan_model("random_effects.stan")

stan_fit <- stan_model$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 1 # print update every 500 iters
)

stan_fit$save_object(file = paste0(output_folder, "/stan_fit.RDS"))
# stan_fit_2 <- readRDS(paste0(output_folder, "/stan_fit.RDS"))

stan_draws = stan_fit$draws(
  variables = c(
    "main_coefficients[2]"
  ),
  format = "df"
)

life_expectancy_coefficients <-
  stan_draws$`main_coefficients[2]`  /
  life_expectancy_standard_deviation *
  log_price_of_life_standard_deviation

hist(life_expectancy_coefficients, breaks = 50)

tibble(
  Specification = "random effects",
  `2.5%` = 
    quantile(life_expectancy_coefficients, 0.025),
  `97.5%` =
    quantile(life_expectancy_coefficients, 0.975)
)

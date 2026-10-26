# Fits the models and builds the tables and figures for CSL.Rtex.
# Requires processing.R, and the Stan fit from bayesian_code.R.

library(forcats)
library(ggplot2)
library(plm, warn.conflicts = FALSE)

example_country <- "United Kingdom"
head_tail_size <- 3
minimum_years <- 5

format_regressors <- function(data) {
  data |>
    stri_replace_all_fixed("_", " ") |>
    stri_replace_all_fixed("percent", "%")
}

format_percent <- function(values) {
  paste0(signif(values, display_figures), "%")
}

format_individually <- function(values) {
  sapply(values, function(value) {
    signif(value, display_figures) |> format()
  })
}

all_variables_table <-
  summarized_prices |>
  filter_price_data() |>
  select(country, year) |>
  distinct() |>
  left_join(world_bank_data, by = c("country", "year")) |>
  pivot_longer(
    c(-year, -country),
    names_to = "variable",
    values_to = "value"
  ) |>
  group_by(variable) |>
  summarize(
    percent_missing = (sum(is.na(value)) / n() * 100),
    percent_positive =
      (sum(value > 0, na.rm = TRUE) / sum(!is.na(value)) * 100),
    .groups = "drop"
  ) |>
  mutate(
    formatted_percent_missing = format_percent(percent_missing),
    formatted_percent_positive = format_percent(percent_positive)
  ) |>
  left_join(variable_data, by = "variable") |>
  mutate(
    # these variables are not from the World Bank
    variable_code = case_when(
      variable == "PPP_health" ~ "9080000:ACTUAL HEALTH",
      variable == "life_expectancy" ~ "WHOSIS_000002",
      .default = variable_code
    )
  )

variable_table <-
  all_variables_table |>
  filter(!(variable %in% paste0("log_", logged_controls))) |>
  mutate(variable = format_regressors(variable)) |>
  arrange(percent_missing) |>
  select(
    Variable = variable,
    Code = variable_code,
    `% missing` = formatted_percent_missing
  )

logged_variable_table <-
  all_variables_table |>
  filter(variable %in% logged_controls) |>
  mutate(variable = format_regressors(variable)) |>
  arrange(percent_positive) |>
  select(
    Variable = variable,
    `% positive` = formatted_percent_positive
  )

controls_table <-
  tibble(control = all_controls) |>
  mutate(
    status = case_when(
      !(control %in% estimable_controls) ~ "unestimable",
      !(control %in% unique_controls) ~ "multicollinear",
      control %in% significant_controls ~ "significant",
      .default = "insignificant"
    )
  ) |>
  arrange(status, control) |>
  select(Control = control, Status = status) |>
  mutate(Control = format_regressors(Control))

make_panel <- function(data) {
  pdata.frame(data, index = c("country", "year"))
}

get_plm_confidence_intervals <- function(model, alpha = 0.05) {
  model_summary <- summary(model, vcov = vcovHC(model, cluster = "group"))
  critical_z <- qnorm(1 - alpha / 2)
  coefficients_table <- model_summary$coefficients

  if (!("z-value" %in% colnames(coefficients_table))) {
    stop("Requires a z distribution")
  }

  coefficients_table |>
    as_tibble(rownames = "coefficient") |>
    mutate(
      lower_bound = Estimate - critical_z * `Std. Error`,
      upper_bound = Estimate + critical_z * `Std. Error`
    )
}

get_plm_interval <- function(model, parameter = "life_expectancy") {
  confidence_intervals <-
    get_plm_confidence_intervals(model) |>
    filter(coefficient == parameter)

  tibble(
    `2.5%` = signif(confidence_intervals$lower_bound, display_figures),
    `50%` = signif(coef(model)[[parameter]], display_figures),
    `97.5%` = signif(confidence_intervals$upper_bound, display_figures)
  )
}

get_feols_interval <- function(model, parameter = "fit_life_expectancy") {
  confidence_intervals <-
    confint(model) |>
    as_tibble(rownames = "regressor") |>
    filter(regressor == parameter)

  tibble(
    `2.5%` = signif(confidence_intervals$`2.5 %`, display_figures),
    `50%` = signif(coef(model)[[parameter]], display_figures),
    `97.5%` = signif(confidence_intervals$`97.5 %`, display_figures)
  )
}

random_effects_model <-
  plm(
    make_plm_formula(unique_controls),
    data = make_panel(default_data),
    model = "random"
  )

random_residuals <- resid(random_effects_model)
random_index <- attr(random_residuals, "index")

# unit root tests need countries with enough years of data
filtered_panel <-
  tibble(
    residuals = as.numeric(random_residuals),
    country = as.character(random_index$country),
    year = as.numeric(random_index$year)
  ) |>
  group_by(country) |>
  filter(n() >= minimum_years) |>
  ungroup() |>
  make_panel()

filtered_residuals <- filtered_panel$residuals

get_unit_root_p_value <- function(test) {
  purtest(filtered_residuals,
    test = test,
    lags = 1
  )$statistic$p.value
}

stationarity_table <-
  tibble(
    test = c("madwu", "Pm", "invnormal", "logit"),
    Test = c("Maddala Wu", "Modified p", "Inverse normal", "Logit")
  ) |>
  rowwise() |>
  mutate(`p-value` = get_unit_root_p_value(test)) |>
  ungroup() |>
  mutate(`p-value` = format_individually(`p-value`)) |>
  select(Test, `p-value`)

pooled_model <- feols(
  make_feols_formula(unique_controls),
  data = present_data,
  vcov = ~ country
)

multicollinear_model <- feols(
  make_feols_formula(estimable_controls),
  data = present_data,
  vcov = ~ country
)

significant_model <- feols(
  make_feols_formula(significant_controls),
  data = present_significant_data,
  vcov = ~ country
)

# fixest predictions are incorrect
# use ivreg predictions for the same model instead
significant_ivreg_model <- ivreg(
  make_plm_formula(significant_controls),
  data = present_significant_data
)

# percent change in the price of life per year of life expectancy
coefficient_to_percent <- function(coefficient) {
  signif((exp(coefficient) - 1) * 100, display_figures)
}

confidence_intervals <- confint(pooled_model)

lower_bound_percent <- coefficient_to_percent(
  confidence_intervals["fit_life_expectancy", "2.5 %"]
)
upper_bound_percent <- coefficient_to_percent(
  confidence_intervals["fit_life_expectancy", "97.5 %"]
)

prediction_data <-
  with_missing_outcomes |>
  left_join(
    present_significant_data |>
      # need to use the same factors for consistency
      select(year, year_factor) |>
      distinct(),
    by = "year"
  ) |>
  arrange(country, year) |>
  mutate(fit_life_expectancy = life_expectancy)

recent_predictions <-
  prediction_data |>
  mutate(
    prediction = predict(significant_ivreg_model, newdata = prediction_data)
  ) |>
  filter(year == recent_year & !is.na(prediction)) |>
  mutate(price_of_life = exp(prediction)) |>
  arrange(desc(price_of_life)) |>
  mutate(rank = seq_len(n()))

write_csv(recent_predictions, output_path("recent_predictions.csv"))

example_prediction <-
  recent_predictions |>
  filter(country == example_country)

best_prediction <- slice_head(recent_predictions, n = 1)
worst_prediction <- slice_tail(recent_predictions, n = 1)

life_expectancy_coefficient <- coef(pooled_model)[["fit_life_expectancy"]]

intersection_data <-
  bind_rows(
    age_range |>
      mutate(
        price_of_life = exp(
          example_prediction$prediction + (
            life_expectancy - example_prediction$life_expectancy
          ) * life_expectancy_coefficient
        ),
        side = "Marginal cost of life"
      ),
    age_range |>
      mutate(
        price_of_life = exp(example_prediction$prediction),
        side = "Marginal benefit of life"
      )
  ) |>
  mutate(price_of_life_in_thousands = price_of_life / 1000) |>
  rename(
    `Life expectancy` = life_expectancy,
    `Price of life, in thousand International $ / QALY` =
      price_of_life_in_thousands,
    Side = side
  )

rank_table <-
  bind_rows(
    slice_head(recent_predictions, n = head_tail_size),
    slice_tail(recent_predictions, n = head_tail_size)
  ) |>
  mutate(
    price_of_life = paste0(
      "$",
      signif(price_of_life, display_figures) |>
        format(
          big.mark = ",",
          drop0trailing = TRUE,
          scientific = FALSE,
          trim = TRUE
        )
    )
  ) |>
  select(
    Rank = rank,
    Country = country,
    `Marginal cost of life` = price_of_life
  )

# from bayesian_code.R
stan_fit <- readRDS(output_path("stan_fit.RDS"))

stan_draws <- stan_fit$draws(format = "df")

# undo the scaling in bayesian_code.R
bayesian_coefficients <-
  stan_draws$`endogenous_variable_coefficients[1]` /
  sd(indexed_country_years$life_expectancy) *
  sd(indexed_price_data$log_price_of_life)

get_statistics <- function(model) {
  regression_statistics <- fitstat(model, c("rmse", "ar2", "ivf1", "wh"))
  weak_instruments_test <- regression_statistics$ivf1
  wu_hausman_test <- regression_statistics$wh

  tibble(
    Statistic = c(
      "RMSE",
      "Adjusted R squared",
      "Weak-instruments test",
      "Wu-Hausman test"
    ),
    Value = c(
      format_individually(regression_statistics$rmse),
      format_individually(regression_statistics$ar2[["ar2"]]),
      paste0(
        "$F$: ",
        format_individually(weak_instruments_test$stat),
        "\n$p$: ",
        format_individually(weak_instruments_test$p)
      ),
      paste0(
        "$\\chi^2$: ",
        format_individually(wu_hausman_test$stat),
        "\n$p$: ",
        format_individually(wu_hausman_test$p)
      )
    )
  )
}

regression_statistics_table <-
  bind_rows(
    get_statistics(pooled_model) |>
      mutate(specification = "Base"),
    get_statistics(multicollinear_model) |>
      mutate(specification = "Multicollinear"),
    get_statistics(significant_model) |>
      mutate(specification = "Significant")
  ) |>
  pivot_wider(
    names_from = specification,
    values_from = Value
  )

get_coefficient_table <- function(model) {
  summary(model)$coeftable |>
    as_tibble(rownames = "regressor")
}

coefficients_table <-
  bind_rows(
    get_coefficient_table(pooled_model) |>
      mutate(specification = "Base"),
    get_coefficient_table(multicollinear_model) |>
      mutate(specification = "Multicollinear"),
    get_coefficient_table(significant_model) |>
      mutate(specification = "Significant")
  ) |>
  rename(
    `Standard error` = `Std. Error`,
    `p-value` = `Pr(>|t|)`
  ) |>
  mutate(
    Regressor = ifelse(
      regressor == "(Intercept)",
      "intercept",
      regressor
    ) |>
      stri_replace_all_fixed("year_factor", "year=") |>
      format_regressors() |>
      stri_replace_all_fixed("%", "\\%"),
    stars = case_when(
      `p-value` < 0.01 ~ "***",
      `p-value` < 0.05 ~ "**",
      `p-value` < 0.1 ~ "*",
      .default = ""
    ),
    Coefficient = paste0(
      format_individually(Estimate),
      stars,
      "\n(",
      format_individually(`Standard error`),
      ")"
    )
  ) |>
  select(specification, Regressor, Coefficient) |>
  pivot_wider(
    names_from = specification,
    values_from = Coefficient,
    values_fill = ""
  )

model_without_endogeneity <- feols(
  paste0(
    "log_price_of_life ~ ",
    paste_controls(c(unique_controls, "life_expectancy")),
    " | year"
  ) |>
    as.formula(),
  data = default_data,
  vcov = ~ country
)

fixed_effects_model <- feols(
  make_fixed_effects_formula(unique_controls),
  data = present_data,
  vcov = ~ country
)

unique_hausman_p_value <- phtest(
  random_effects_model,
  plm(
    make_plm_formula(unique_controls),
    data = make_panel(default_data),
    model = "within"
  )
)$p.value

# TODO: add to output
serial_correlation_test <- pbgtest(plm(
  make_plm_formula(significant_controls),
  data = make_panel(default_data),
  model = "random"
))

specification_robustness_table <-
  bind_rows(
    get_feols_interval(pooled_model) |>
      mutate(Specification = "pooled model"),
    get_plm_interval(random_effects_model) |>
      mutate(Specification = "random effects model"),
    tibble(
      Specification = "hierarchical bayesian",
      `2.5%` = quantile(bayesian_coefficients, 0.025),
      `50%` = quantile(bayesian_coefficients, 0.5),
      `97.5%` = quantile(bayesian_coefficients, 0.975)
    ),
    get_feols_interval(fixed_effects_model) |>
      mutate(Specification = "fixed effects"),
    get_feols_interval(multicollinear_model) |>
      mutate(Specification = "with multicollinear controls"),
    get_feols_interval(significant_model) |>
      mutate(Specification = "significant model"),
    get_feols_interval(
      model_without_endogeneity,
      parameter = "life_expectancy"
    ) |>
      mutate(Specification = "without endogeneity")
  )

# NA if the model can't be estimated with this data
get_data_coefficient <- function(data) {
  missing_interval <- tibble(`2.5%` = NA, `50%` = NA, `97.5%` = NA)
  tryCatch(
    feols(
      make_feols_formula(unique_controls),
      data = data,
      vcov = ~ country
    ) |>
      get_feols_interval(),
    warning = function(w) missing_interval,
    error = function(e) missing_interval
  )
}

plot_confidence_intervals <- function(table, variable) {
  table[[variable]] <- fct_inorder(table[[variable]])
  ggplot(table) +
    aes(
      x = .data[[variable]],
      y = `50%`,
      ymin = `2.5%`,
      ymax = `97.5%`
    ) +
    ylab("Life expectancy coefficient") +
    labs(title = "95% confidence intervals by specification") +
    geom_pointrange() +
    guides(x = guide_axis(angle = 90))
}

# the life expectancy coefficient for every combination of data choices
robustness_table <-
  full_data |>
  nest_by(
    price_kind,
    price_deflator_kind,
    price_exchange_kind,
    gdp_exchange_kind,
    gdp_deflator_kind,
    .key = "data"
  ) |>
  mutate(
    data = get_data_coefficient(data)
  ) |>
  unnest(data) |>
  ungroup()

robustness_defaults <- c(
  price_kind = default_price_kind,
  price_deflator_kind = default_price_deflator_kind,
  price_exchange_kind = default_price_exchange_kind,
  gdp_exchange_kind = default_gdp_exchange_kind,
  gdp_deflator_kind = default_gdp_deflator_kind
)

# vary one data choice, holding the others at their defaults
vary_data_choice <- function(choice, label) {
  others <- setdiff(names(robustness_defaults), choice)
  robustness_table |>
    semi_join(as_tibble(as.list(robustness_defaults[others])), by = others) |>
    select(-all_of(others)) |>
    rename(!!label := all_of(choice))
}

gdp_deflators_robustness_table <-
  vary_data_choice("gdp_deflator_kind", "GDP deflators")

gdp_exchange_robustness_table <-
  vary_data_choice("gdp_exchange_kind", "GDP exchange rates")

prices_robustness_table <-
  vary_data_choice("price_kind", "Prices")

price_deflators_table <-
  vary_data_choice("price_deflator_kind", "Price deflators")

price_exchange_table <-
  vary_data_choice("price_exchange_kind", "Price exchange rates")

# health PPPs exist only for benchmark years, which weakens the first stage
benchmark_health_ppp_model <- feols(
  make_feols_formula(unique_controls),
  data = full_data |>
    filter_world_bank_data(chosen_gdp_exchange_kind = "PPP health") |>
    filter_price_data(),
  vcov = ~ country
)

benchmark_health_ppp_observations <- nobs(benchmark_health_ppp_model)
benchmark_health_ppp_first_stage_f <-
  fitstat(benchmark_health_ppp_model, "ivf1")$ivf1$stat |>
  signif(display_figures)

map <-
  raw_map_data |>
  select(-map_country, -country_code_2) |>
  # add in the predicted price of life in recent_year
  inner_join(
    recent_predictions |>
      select(country, price_of_life),
    by = "country"
  ) |>
  select(`Predicted marginal cost of life` = price_of_life) |>
  st_transform(crs = "+proj=eqearth")

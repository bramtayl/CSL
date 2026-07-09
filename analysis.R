library(carData) # needed for car
library(timechange) # needed for lubridate

library(car)
library(forcats)
library(fixest)
library(ggplot2)
library(lubridate, warn.conflicts = FALSE)
library(knitr)
library(ivreg)
library(plm)
library(purrr, warn.conflicts = FALSE)
library(readr)
library(rex)
library(rnaturalearth)
suppressPackageStartupMessages(library(sf))
library(stargazer)
library(stringi)
library(tidyr, warn.conflicts = TRUE)

# load last for filter
library(dplyr, warn.conflicts = FALSE)

display_figures <- 3
example_country <- "United Kingdom"
head_tail_size <- 3
minimum_years <- 5
output_folder <- "~/Desktop"
recent_year <- 2015
year_regex <- rex(n_times(digit, 4))

format_regressors <- function(data) {
  data |>
    stri_replace_all_fixed("_", " ") |>
    stri_replace_all_fixed("percent", "%")
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
    formatted_percent_missing =
    percent_missing |>
    signif(display_figures) |>
    paste0("%"),
    formatted_percent_positive =
    percent_positive |>
    signif(display_figures) |>
    paste0("%"),
  ) |>
  left_join(variable_data, by = "variable") |>
  mutate(
    variable_code = ifelse(
      variable == "PPP_health",
      "9080000:ACTUAL HEALTH",
      ifelse(
        variable == "life_expectancy",
        "WHOSIS_000002",
        variable_code
      )
    )
  )

variable_table <-
  all_variables_table |>
  anti_join(
    tibble(variable = paste0("log_", logged_controls)),
    by = c("variable")
  ) |>
  mutate(variable = format_regressors(variable)) |>
  arrange(percent_missing) |>
  select(
    Variable = variable,
    Code = variable_code,
    `% missing` = formatted_percent_missing
  )

logged_variable_table <-
  all_variables_table |>
  semi_join(
    tibble(variable = logged_controls),
    by = c("variable")
  ) |>
  mutate(variable = format_regressors(variable)) |>
  arrange(percent_positive) |>
  select(
    Variable = variable,
    `% positive` = formatted_percent_positive
  )

# TODO: add to output
controls_table <-
  tibble(control = all_controls) |>
  left_join(
    tibble(
      control = estimable_controls,
      estimable = TRUE
    ),
    by = "control"
  ) |>
  left_join(
    tibble(
      control = unique_controls,
      collinear = FALSE
    ),
    by = "control"
  ) |>
  left_join(
    tibble(
      control = significant_controls,
      significant = TRUE
    ),
    by = "control"
  ) |>
  anti_join(tibble(control = logged_controls), by = "control") |>
  mutate(
    estimable = coalesce(estimable, FALSE),
    collinear = coalesce(collinear, TRUE),
    significant = coalesce(significant, FALSE),
    status = ifelse(
      estimable,
      ifelse(collinear,
        "multicollinear",
        ifelse(
          significant,
          "signficant",
          "insignificant"
        )
      ),
      "unestimable"
    )
  ) |>
  arrange(status, control) |>
  select(-estimable, -collinear, -significant) |>
  rename(Control = control, Status = status) |>
  mutate(Control = format_regressors(Control))

make_plm_formula <- function(controls) {
  if (length(controls) == 0) {
    controls_string <- ""
  } else {
    controls_string <-
      controls |>
        paste0(collapse = " + ") %>%
        paste0(" + ", .)
  }
  paste0(
    "log_price_of_life ~ life_expectancy",
    controls_string,
    " + year_factor | log_gdp_per_capita",
    controls_string,
    " + year_factor"
  ) |>
    as.formula()
}

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
    `2.5%` =
      confidence_intervals$lower_bound |>
      signif(display_figures),
    `50%` = coef(model)[[parameter]] |> signif(display_figures),
    `97.5%` =
      confidence_intervals$upper_bound |>
      signif(display_figures)
  )
}

# TODO: add to output
random_effects_model <-
  plm(
    make_plm_formula(unique_controls),
    data = make_panel(default_data),
    model = "random"
  )

random_residuals <- resid(random_effects_model)
random_index <- attr(random_residuals, "index")

filtered_residuals <-
  tibble(
    residuals = as.numeric(random_residuals),
    country = as.character(random_index$country),
    year = as.numeric(random_index$year)
  ) |>
  group_by(country) |>
  filter(n() >= minimum_years) |>
  ungroup() |>
  pdata.frame(index = c("country", "year")) %>%
  .$residuals

get_unit_root_p_value <- function(test) {
  purtest(filtered_residuals,
    test = test,
    lags = 1
  )$statistic$p.value
}

format_individually <- function(values) {
  sapply(values, function(value) {
    signif(value, display_figures) |> format()
  })
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

significant_model <- feols(
  make_feols_formula(significant_controls),
  data = present_significant_data,
  vcov = ~ country
)

significant_ivreg_model <- ivreg(
  make_plm_formula(significant_controls),
  data = present_significant_data
)

confidence_intervals <- confint(pooled_model)

lower_bound_percent <- signif((exp(confidence_intervals["fit_life_expectancy", "2.5 %"]) - 1) * 100, display_figures)
upper_bound_percent <- signif((exp(confidence_intervals["fit_life_expectancy", "97.5 %"]) - 1) * 100, display_figures)

extra_model <- feols(
  make_feols_formula(estimable_controls),
  data = present_data,
  vcov = ~ country
)

prediction_data <-
  with_missing_outcomes |>
  select(-year_factor) |>
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
  with_missing_outcomes |>
  select(-year_factor) |>
  left_join(
    present_significant_data |>
      # need to use the same factors for consistency
      select(year, year_factor) |>
      distinct(),
    by = "year"
  ) |>
  arrange(country, year) |>
  mutate(fit_life_expectancy = life_expectancy) %>%
  # fixest predictions are incorrect
  # use ivreg predictions for the same model instead
  mutate(prediction = predict(significant_ivreg_model, newdata = .)) |>
  filter(year == recent_year & !is.na(prediction)) |>
  mutate(price_of_life = exp(prediction)) |>
  arrange(desc(price_of_life)) |>
  mutate(rank = seq_len(n()))

write_csv(
  recent_predictions,
  paste0(output_folder, "/recent_predictions.csv")
)

example_prediction <-
  recent_predictions |>
  filter(country == example_country)

best_prediction <- recent_predictions |> slice(1)
worst_prediction <- recent_predictions %>% slice(nrow(.))

model_coefficients <- coef(pooled_model)
life_expectancy_coefficient <- model_coefficients[["fit_life_expectancy"]]

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
    recent_predictions |> slice_head(n = head_tail_size),
    recent_predictions |> slice_tail(n = head_tail_size)
  ) |>
  mutate(
    price_of_life =
    signif(price_of_life, display_figures) |>
    format(
      big.mark = ",",
      drop0trailing = TRUE,
      scientific = FALSE,
      trim = TRUE
    ) %>%
    paste0("$", .)
  ) |>
  select(
    Rank = rank,
    Country = country,
    `Marginal cost of life` = price_of_life
  )

pooled_summary <- summary(pooled_model)

stan_fit <- readRDS(paste0(output_folder, "/stan_fit.RDS"))

stan_draws = stan_fit$draws(
  format = "df"
)

bayesian_coefficients <-
  stan_draws$`endogenous_variable_coefficients[1]`  /
  sd(indexed_country_years$life_expectancy) *
  sd(indexed_price_data$log_price_of_life)

get_statistics <- function(model) {
  regression_statistics <- fitstat(model, c("rmse", "ar2", "ar2", "ivf1", "wh"))
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

multicollinear_model <- feols(
  make_feols_formula(estimable_controls),
  data = present_data,
  vcov = ~ country
)

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

get_coefficient_table = function(model) {
  model |>
  summary() %>%
  .$coeftable |>
  as_tibble(rownames = "regressor")
}

# multicollinear_model
# significant_model
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
  rowwise() |>
  mutate(
    Regressor = ifelse(
      regressor == "(Intercept)",
      "intercept",
      regressor
    ) |>
      stri_replace_all_fixed("year_factor", "year=") |>
      format_regressors() |>
      stri_replace_all_fixed("%", "\\%"),
    Coefficient = paste0(
      signif(Estimate, display_figures) |> format(),
      ifelse(
        `p-value` < 0.1,
        ifelse(
          `p-value` < 0.05,
          ifelse(
            `p-value` < 0.01,
            "***",
            "**"
          ),
          "*"
        ),
        ""
      ),
      "\n(",
      signif(`Standard error`, display_figures) |> format(),
      ")"
    )
  ) |>
  ungroup() |>
  select(specification, Regressor, Coefficient) |>
  pivot_wider(
    names_from = specification,
    values_from = Coefficient,
    values_fill = ""
  )

simple_formula <-
  c(unique_controls, "life_expectancy") |>
  paste0(collapse = " + ") %>%
  paste0(
    "log_price_of_life ~ ",
    .,
    " | year"
  ) |>
  as.formula()

model_without_endogeneity <- feols(
  simple_formula,
  data = default_data,
  vcov = ~ country
)

fixed_effects_model <- feols(
  make_fixed_effects_formula(unique_controls),
  data = present_data,
  vcov = ~ country
)

get_feols_interval <- function(model, parameter = "fit_life_expectancy") {
  confidence_intervals <-
    confint(model) |>
    as_tibble(rownames = "regressor") |>
    filter(regressor == parameter)

  tibble(
    `2.5%` =
      confidence_intervals$`2.5 %` |>
      signif(display_figures),
    `50%` =  coef(model)[[parameter]] |> signif(display_figures),
    `97.5%` =
      confidence_intervals$`97.5 %` |>
      signif(display_figures)
  )
}

unique_hausman_p_value <- phtest(
  random_effects_model,
  plm(
    make_plm_formula(unique_controls),
    data = make_panel(default_data),
    model = "within"
  )
)$p.value

# TODO: add to output
pbgtest(plm(
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
      `2.5%` = 
        quantile(bayesian_coefficients, 0.025),
      `50%` = 
        quantile(bayesian_coefficients, 0.5),
      `97.5%` =
        quantile(bayesian_coefficients, 0.975)
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

get_data_coefficient <- function(data) {
  tryCatch(
    {
      feols(
        make_feols_formula(unique_controls),
        data = data,
        vcov = ~ country
      ) |>
        get_feols_interval()
    },
    warning = function(w) {
      tibble(
        `2.5%` = NA,
        `50%` = NA,
        `97.5%` = NA
      )
    },
    error = function(e) {
      tibble(
        `2.5%` = NA,
        `50%` = NA,
        `97.5%` = NA
      )
    }
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

gdp_deflators_robustness_table <-
  robustness_table |>
  filter(
    price_kind == default_price_kind &
      price_deflator_kind == default_price_deflator_kind &
      price_exchange_kind == default_price_exchange_kind &
      gdp_exchange_kind == default_gdp_exchange_kind
  ) |>
  select(
    -price_kind,
    -price_deflator_kind,
    -price_exchange_kind,
    -gdp_exchange_kind
  ) |>
  rename(`GDP deflators` = gdp_deflator_kind)

gdp_exchange_robustness_table <-
  robustness_table |>
  filter(
    price_kind == default_price_kind &
      price_deflator_kind == default_price_deflator_kind &
      price_exchange_kind == default_price_exchange_kind &
      gdp_deflator_kind == default_gdp_deflator_kind
  ) |>
  select(
    -price_kind,
    -price_deflator_kind,
    -price_exchange_kind,
    -gdp_deflator_kind
  ) |>
  rename(`GDP exchange rates` = gdp_exchange_kind)

prices_robustness_table <-
  robustness_table |>
  filter(
    price_deflator_kind == default_price_deflator_kind &
      price_exchange_kind == default_price_exchange_kind &
      gdp_exchange_kind == default_gdp_exchange_kind &
      gdp_deflator_kind == default_gdp_deflator_kind
  ) |>
  select(
    -price_deflator_kind,
    -price_exchange_kind,
    -gdp_deflator_kind,
    -gdp_exchange_kind
  ) |>
  rename(Prices = price_kind)

price_deflators_table <-
  robustness_table |>
  filter(
    price_kind == default_price_kind &
      price_exchange_kind == default_price_exchange_kind &
      gdp_exchange_kind == default_gdp_exchange_kind &
      gdp_deflator_kind == default_gdp_deflator_kind
  ) |>
  select(
    -price_kind,
    -price_exchange_kind,
    -gdp_deflator_kind,
    -gdp_exchange_kind
  ) |>
  rename(`Price deflators` = price_deflator_kind)

price_exchange_table <-
  robustness_table |>
  filter(
    price_kind == default_price_kind &
      price_deflator_kind == default_price_deflator_kind &
      gdp_exchange_kind == default_gdp_exchange_kind &
      gdp_deflator_kind == default_gdp_deflator_kind
  ) |>
  select(
    -price_kind,
    -price_deflator_kind,
    -gdp_deflator_kind,
    -gdp_exchange_kind
  ) |>
  rename(`Price exchange rates` = price_exchange_kind)

map <-
  raw_map_data %>%
  select(-map_country, -country_code_2) %>%
  # add in life expectancy in recent_year
  inner_join(
    recent_predictions %>%
      select(country, price_of_life),
    by = "country"
  ) %>%
  select(`Predicted marginal cost of life` = price_of_life) %>%
  st_transform(crs = "+proj=eqearth")

# plot(map, border = NA, key.pos = 1, main = NULL, logz = TRUE)
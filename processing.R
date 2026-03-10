library(carData) # needed for car
library(timechange) # needed for lubridate

library(car)
library(fixest)
library(lubridate, warn.conflicts = FALSE)
library(knitr)
library(ivreg)
library(plm)
library(purrr, warn.conflicts = FALSE)
library(readr)
library(rex)
library(rnaturalearth)
suppressPackageStartupMessages(library(sf))
library(stringi)
library(tidyr, warn.conflicts = TRUE)

# load last for filter
library(dplyr, warn.conflicts = FALSE)

default_gdp_exchange_kind <- "PPP GDP"
default_gdp_deflator_kind <- "GDP deflator"
default_price_exchange_kind <- "PPP GDP"
default_price_deflator_kind <- "GDP deflator"
default_price_kind <- "article prices"

deflator_year <- 2024
display_figures <- 3
minimum_presence <- 0
output_folder <- "~/Desktop"
recent_year <- 2015
year_regex <- rex(n_times(digit, 4))

filter_world_bank_data <- function(data,
  chosen_gdp_exchange_kind = default_gdp_exchange_kind,
  chosen_gdp_deflator_kind = default_gdp_deflator_kind
) {
  data |>
    filter(
      gdp_exchange_kind == chosen_gdp_exchange_kind &
        gdp_deflator_kind == chosen_gdp_deflator_kind
    ) |>
    select(
      -gdp_exchange_kind,
      -gdp_deflator_kind
    )
}

filter_price_data <- function(data,
  chosen_price_kind = default_price_kind,
  chosen_price_exchange_kind = default_price_exchange_kind,
  chosen_price_deflator_kind = default_price_deflator_kind
) {
  data |>
    filter(
      price_kind == chosen_price_kind &
        price_exchange_kind == chosen_price_exchange_kind &
        price_deflator_kind == chosen_price_deflator_kind
    ) |>
    select(
      -price_kind,
      -price_exchange_kind,
      -price_deflator_kind
    )
}

variable_data <-
  read_csv(
    "data/selected_world_bank_variables.csv",
    show_col_types = FALSE,
    na = ""
  )

raw_long_world_bank_data <-
  read_csv("data/world_bank_data.csv", show_col_types = FALSE, na = "") |>
  select(
    # 2 digit country codes
    country_code_2 = iso2c,
    variable_code = indicator_id,
    long_name = indicator,
    value,
    # 3 digit world bank codes
    world_bank_country_code_3 = iso3c,
    world_bank_country = country,
    year = date
  ) |>
  left_join(variable_data, by = "variable_code")

na_log <- function(variable) {
  logged <- log(variable)
  ifelse(is.infinite(logged), NA, logged)
}

# TODO: add to output

logged_controls <- c(
  "alcohol_consumption_per_capita",
  "population_density",
  "precipitation"
)

logged_world_bank_data <-
  raw_long_world_bank_data |>
  semi_join(
    tibble(variable = logged_controls),
    by = c("variable")
  ) |>
  mutate(
    value = na_log(value),
    variable = paste0("log_", variable)
  )

# all countries from all 3 datasets
# the World Bank data includes a "euro area" country
# which I need currency data for
raw_country_data <-
  read_csv("data/countrycode_data.csv", show_col_types = FALSE, na = "") |>
  select(
    package_country = country.name.en,
    country_code_2 = iso2c,
    package_country_code_3 = iso3c
  ) |>
  # with no country code, we can't match
  filter(!is.na(country_code_2)) |>
  # add in world bank countries
  full_join(
    raw_long_world_bank_data |>
      select(
        country_code_2,
        world_bank_country_code_3,
        world_bank_country
      ) |>
      distinct(),
    by = "country_code_2"
  ) |>
  mutate(
    country = coalesce(
      package_country,
      world_bank_country
    ),
    country_code_3 = coalesce(
      package_country_code_3,
      world_bank_country_code_3
    )
  )

# sanity checks
if (nrow(
  raw_country_data |>
    filter(
      !is.na(world_bank_country_code_3) &
        !is.na(package_country_code_3) &
        world_bank_country_code_3 != package_country_code_3
    )
) > 0) {
  stop("Country code 3 mismatch")
}

if (any(duplicated(raw_country_data$country))) {
  stop("Repeated countries")
}

if (any(is.na(raw_country_data$country))) {
  stop("Missing countries")
}

country_data <-
  raw_country_data |>
  select(
    -package_country,
    -world_bank_country,
    -package_country_code_3,
    -world_bank_country_code_3
  )

raw_hale_data <- read_csv(
  "data/health_adjusted_life_expectancy.csv",
  show_col_types = FALSE, 
  na = ""
) |>
  select(
    indicator = Indicator,
    country_code_3 = SpatialDimValueCode,
    sex = Dim1,
    value = FactValueNumeric,
    year = Period
  ) |>
  filter(
    indicator == "Healthy life expectancy (HALE) at birth (years)" &
      sex == "Both sexes"
  ) |>
  select(-indicator, -sex) |>
  left_join(
    country_data |>
      select(country, country_code_3)
  ) |>
  mutate(variable = "life_expectancy")

if (nrow(
  raw_hale_data |>
    filter(is.na(country))
) > 0) {
  stop("Missing HALE countries")
}

write_csv(country_data, paste0(output_folder, "/country_data.csv"))

clean_health_ppps <- function(base_year, number_of_rows) {
  base_year_data <-
    paste0("data/health_", base_year, ".csv") |>
    read_csv(n_max = number_of_rows, na = "..", show_col_types = FALSE) |>
    select(
      -`Classification Name`,
      -`Classification Code`,
      -`Series Name`,
      -`Series Code`,
    ) |>
    rename(
      country_code_3 = `Country Code`,
      icp_country = `Country Name`
    )
  
  old_names <- names(base_year_data)
  year_columns <- stri_detect_regex(old_names, year_regex)

  # get the year at the start
  names(base_year_data)[year_columns] <-
    old_names[year_columns] |>
    stri_match_first_regex(year_regex)

  base_year_data |>
    select(-icp_country) |>
    pivot_longer(
      -country_code_3,
      names_to = "year",
      values_to = "PPP_health",
      values_drop_na = TRUE,
      names_transform = as.numeric
    ) |>
    left_join(
      base_year_data |>
        select(country_code_3, icp_country),
      by = "country_code_3"
    )
}

raw_ppp_health_data <-
  tibble(
    base_year = c(2005, 2011, 2017, 2021),
    number_of_rows = c(195, 222, 222, 207)
  ) |>
  group_by(base_year) |>
  reframe(clean_health_ppps(base_year, number_of_rows)) |>
  mutate(
    # fix a typo in the data
    country_code_3 = ifelse(
      icp_country == "Russian Federation",
      "RUS",
      country_code_3
    )
  ) |>
  # for a given year, use data from latest base year
  group_by(country_code_3, year) |>
  arrange(desc(base_year)) |>
  slice(1) |>
  ungroup() |>
  select(-base_year) |>
  left_join(
    country_data |>
      select(country, country_code_3),
    by = "country_code_3"
  )

# check to make sure we aren't losing important countries
raw_ppp_health_data |>
  filter(is.na(country)) |>
  write_csv(paste0(output_folder, "/missing_PPP_countries.csv"))

world_bank_data <-
  bind_rows(
    raw_long_world_bank_data,
    logged_world_bank_data
  ) |>
  select(
    country_code_2,
    year,
    variable,
    value
  ) |>
  left_join(
    country_data |>
      select(country, country_code_2),
    by = "country_code_2"
  ) |>
  select(-country_code_2) |>
  bind_rows(
    raw_ppp_health_data |>
      select(-country_code_3, -icp_country) |>
      filter(!is.na(country)) |>
      rename(value = PPP_health) |>
      mutate(variable = "PPP_health"),
  ) |>
  bind_rows(
    raw_hale_data |>
      select(-country_code_3)
  ) |>
  pivot_wider(
    names_from = variable,
    values_from = value
  )

deflator_data <-
  bind_rows(
    read_csv("data/GDP_deflator.csv", na = "", show_col_types = FALSE) |>
      select(date, deflator = value) |>
      mutate(deflator_kind = "GDP deflator"),
    read_csv("data/CPI.csv", na = "", show_col_types = FALSE) |>
      select(date, deflator = value) |>
      mutate(deflator_kind = "CPI"),
    read_csv("data/CPI_medical.csv", na = "", show_col_types = FALSE) |>
      select(date, deflator = value) |>
      mutate(deflator_kind = "CPI medical")
  ) |>
  # average by year
  mutate(year = year(date)) |>
  group_by(deflator_kind, year) |>
  summarize(deflator = mean(deflator), .groups = "drop") |>
  group_by(deflator_kind) |>
  # USD in recent_year / basket
  # * basket / historical USD
  # = USD in recent_year / historical USD
  mutate(deflator_ratio = deflator[year == deflator_year] / deflator) |>
  ungroup() |>
  select(-deflator)

currency_ratios <-
  world_bank_data |>
  select(country, year,
    `nominal exchange rate` = nominal_exchange_rate,
    `PPP GDP` = PPP_GDP,
    `PPP private consumption` = PPP_private_consumption,
    `PPP health` = PPP_health
  ) |>
  pivot_longer(c(-country, -year),
    names_to = "exchange_rate_kind",
    # = local historical currency / historical USD
    values_to = "exchange_rate",
    values_drop_na = TRUE
  ) |>
  # duplicate for different price adjustments
  inner_join(deflator_data, by = "year", relationship = "many-to-many") |>
  mutate(
    # * historical USD / local historical currency
    # * USD in recent_year / historical USD
    # = USD in recent_year / local historical currency
    currency_ratio = 1 / exchange_rate * deflator_ratio
  )

country_replacements <- read_csv(
  "data/country_replacements.csv",
  show_col_types = FALSE,
  na = ""
)

currency_country_replacements <- read_csv(
  "data/currency_country_replacements.csv",
  show_col_types = FALSE,
  na = ""
)

only_positive <- function(vector) {
  vector |>
    as.numeric() %>%
    # must be positive
    ifelse(. <= 0, NA, .)
}

raw_prices <-
  read_csv(
    "data/ratios.csv",
    show_col_types = FALSE,
    col_types = cols_only(
      `Article ID` = col_character(),
      `Issue Year` = col_double(),
      `$/QALY from Article` = col_character(),
      `$/DALY from Article` = col_character(),
      `$/QALY Final` = col_character(),
      `$/QALY Final in Current USD` = col_character()
    )
  ) |>
  rename(
    article_id = `Article ID`,
    year = `Issue Year`,
    local_price_of_life_qaly = `$/QALY from Article`,
    local_price_of_life_daly = `$/DALY from Article`,
    local_price_of_life_qaly_final = `$/QALY Final`
  ) |>
  mutate(
    local_price_of_life_qaly = only_positive(local_price_of_life_qaly),
    local_price_of_life_daly = only_positive(local_price_of_life_daly),
    local_price_of_life_qaly_final = only_positive(
      local_price_of_life_qaly_final
    ),
    local_price_of_life_both = coalesce(
      local_price_of_life_qaly,
      local_price_of_life_daly
    ),
    # 0 means NA
    year = as.numeric(year) %>% ifelse(. == 0, NA, .),
    ratio_id = seq_len(n())
  )

raw_methods <-
  read_csv(
    "data/methods.csv",
    show_col_types = FALSE,
    col_types = cols_only(
      `Article ID` = col_character(),
      `Countries` = col_character(),
      `Currency Country` = col_character(),
      `Currency Year` = col_integer()
    ),
    name_repair = "minimal"
  ) |>
  rename(
    article_id = `Article ID`,
    tufts_country = `Countries`,
    tufts_currency_country = `Currency Country`,
    currency_year = `Currency Year`
  ) |>
  distinct() |> # remove identical rows
  group_by(article_id) |> # use most recent entry for duplicated rows
  slice_tail(n = 1) |>
  ungroup() |>
  mutate(
    # this should be ok to do because, by definition,
    # the US PPP exchange rate and nominal exchange rate are both 1
    tufts_currency_country = ifelse(
      tufts_currency_country == "International",
      "United States",
      tufts_currency_country
    ),
    currency_year = as.numeric(currency_year),
  ) |>
  # replace countries
  left_join(country_replacements, by = "tufts_country") |>
  mutate(country = coalesce(country_replacement, tufts_country)) |>
  select(-country_replacement, -tufts_country) |>
  # replace currency countries
  left_join(currency_country_replacements, by = "tufts_currency_country") |>
  mutate(currency_country = coalesce(
    currency_country_replacement,
    tufts_currency_country
  )) |>
  select(-tufts_currency_country, -currency_country_replacement)

# check to see if we should add more country replacements
raw_methods |>
  select(country) |>
  distinct() |>
  arrange(country) |>
  filter(!is.na(country)) |>
  anti_join(
    country_data,
    by = "country"
  ) |>
  write_csv(paste0(output_folder, "/unmatched_countries.csv"))

# check to see if we should add more currency country replacements
raw_methods |>
  select(currency_country) |>
  distinct() |>
  arrange(currency_country) |>
  filter(!is.na(currency_country)) |>
  anti_join(
    country_data |> select(currency_country = country),
    by = "currency_country"
  ) |>
  write_csv(paste0(output_folder, "/unmatched_currency_countries.csv"))

methods <-
  raw_methods |>
  semi_join(
    country_data |>
      select(country),
    by = "country"
  )

expanded_prices <-
  raw_prices |>
  select(
    ratio_id,
    `article prices` = local_price_of_life_qaly,
    `with DALY prices` = local_price_of_life_both,
    `with reader adjustments` = local_price_of_life_qaly_final
  ) |>
  pivot_longer(
    -ratio_id,
    names_to = "price_kind",
    values_to = "local_price_of_life",
    values_drop_na = TRUE
  ) |>
  left_join(
    raw_prices |> select(article_id, ratio_id, year),
    by = "ratio_id"
  ) |>
  select(-ratio_id)

methods_with_currency_ratios <-
  inner_join(
    methods |>
      select(article_id, country, currency_country, currency_year),
    currency_ratios |>
      select(
        currency_country = country,
        currency_year = year,
        price_deflator_kind = deflator_kind,
        price_exchange_kind = exchange_rate_kind,
        price_currency_ratio = currency_ratio
      ),
    by = c("currency_year", "currency_country"),
    # duplicate for deflator/exchange rate kinds
    relationship = "many-to-many"
  ) |>
  select(-currency_country, -currency_year)

# ratios with matching countries and currency countries
transformed_prices <-
  inner_join(
    expanded_prices,
    methods_with_currency_ratios,
    by = "article_id",
    # duplicate for deflator/exchange rate/ratio kinds
    relationship = "many-to-many"
  ) |>
  select(-article_id) |>
  mutate(
    # local historical currency
    # * USD in recent_year / local historical currency
    # = USD in recent_year
    log_price_of_life = log(local_price_of_life * price_currency_ratio)
  )

summarized_prices <-
  transformed_prices |>
  group_by(
    price_kind,
    price_deflator_kind,
    price_exchange_kind,
    country,
    year
  ) |>
  summarize(
    log_price_of_life = mean(log_price_of_life),
    number_of_observations = n(),
    .groups = "drop"
  ) |>
  arrange(
    price_kind,
    price_deflator_kind,
    price_exchange_kind,
    country,
    year
  )

world_bank_data_transformed <-
  world_bank_data |>
  select(
    -nominal_exchange_rate,
    -PPP_GDP,
    -PPP_private_consumption,
    -PPP_health
  ) |>
  select(
    -alcohol_consumption_per_capita,
    -population_density,
    -precipitation
  ) |>
  inner_join(
    currency_ratios |>
      rename(
        gdp_exchange_kind = exchange_rate_kind,
        gdp_deflator_kind = deflator_kind,
        gdp_currency_ratio = currency_ratio
      ),
    by = c("country", "year"),
    # duplicate for different price adjustments
    relationship = "many-to-many"
  ) |>
  mutate(
    # local historical currency
    # * USD in recent_year / local historical currency
    # = USD in recent_year
    log_gdp_per_capita = log(local_GDP_per_capita * gdp_currency_ratio)
  ) |>
  select(-gdp_currency_ratio) |>
  arrange(gdp_exchange_kind, gdp_deflator_kind)

with_missing_outcomes <-
  filter_world_bank_data(world_bank_data_transformed) |>
  left_join(
    filter_price_data(summarized_prices),
    by = c("country", "year")
  ) |>
  mutate(year_factor = as.factor(year))

default_data <-
  with_missing_outcomes |>
  filter(!is.na(log_price_of_life)) |>
  mutate(year_factor = as.factor(year))

default_data |>
  write_csv(paste0(output_folder, "/default_data.csv"))

all_controls <- c(
  "GDP_growth_rate",
  "Gini_index",
  "log_alcohol_consumption_per_capita",
  "log_population_density",
  "log_precipitation",
  "percent_child_obesity",
  "percent_college_enrollment",
  "percent_electricity_access",
  "percent_female_to_male_employment_ratio",
  "percent_GDP_from_farming",
  "percent_GDP_from_industry",
  "percent_health_spending_external",
  "percent_health_spending_public",
  "percent_primary_school_completion",
  "population_growth_rate",
  "unemployment_rate"
)

if (length(setdiff(all_controls, names(default_data))) != 0) {
  stop("Controls mismatch")
}

make_feols_formula <- function(controls) {
  if (length(controls) == 0) {
    controls_string <- "1"
  } else {
    controls_string <-
      paste0(controls, collapse = " + ")
  }

  paste0(
    "log_price_of_life ~ ",
    controls_string,
    " | year | life_expectancy ~ log_gdp_per_capita"
  ) |>
    as.formula()
}

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

make_fixed_effects_formula <- function(controls) {
  if (length(controls) == 0) {
    controls_string <- "1"
  } else {
    controls_string <-
      paste0(controls, collapse = " + ")
  }

  paste0(
    "log_price_of_life ~ ",
    controls_string,
    " | country + year | life_expectancy ~ log_gdp_per_capita"
  ) |>
    as.formula()
}

prioritize_most_useful <- function(controls, max_controls = length(controls)) {
  included_controls <- character(0)
  while (length(included_controls) < max_controls) {
    left_over <- setdiff(controls, included_controls)
    r_squareds <- sapply(left_over, function(control) {
      tryCatch({
        ivreg(
          make_plm_formula(c(control, included_controls)),
          data = default_data
        ) |>
          summary() %>%
          .[["adj.r.squared"]]
      }, warning = function(w) {
        0.0
      }, error = function(e) {
        0.0
      })
    }) |>
      sort(decreasing = TRUE)

    best_control <- names(r_squareds)[1]
    r_squared <- r_squareds[1]
    if (r_squared <= 0) {
      break
    }
    included_controls <- c(included_controls, best_control)
  }
  included_controls
}

estimable_controls <- prioritize_most_useful(all_controls)

remove_max_vif <- function(controls, cutoff) {
  while (length(controls) > 0) {
    model <- ivreg(
      make_plm_formula(controls),
      data = default_data
    )

    vifs <- vif(model)[,"GVIF^(1/(2*Df))"]

    vifs_table <-
      tibble(
        coefficient = names(vifs),
        value = vifs
      ) |>
      left_join(
        tibble(coefficient = controls, is_control = TRUE),
        by = "coefficient"
      ) |>
      filter(is_control | coefficient == "life_expectancy") |>
      arrange(desc(value))

    removed <-
      vifs_table |>
      filter(is_control) %>%
      .$coefficient %>%
      .[1]

    if (vifs_table$value[1] > cutoff) {
      controls <- setdiff(controls, removed)
    } else {
      break
    }
  }
  sort(controls)
}

unique_controls <- remove_max_vif(estimable_controls, sqrt(5))

get_significant <- function(controls, cutoff) {
  while (length(controls) > 0) {
    model <- feols(
      make_feols_formula(controls),
      data = default_data,
      vcov = ~ country
    )

    coefficients_table <-
      tibble(
        summary(model)$coeftable %>%
        as_tibble(rownames = "coefficient")
      ) |>
      semi_join(
        tibble(coefficient = controls),
        by = "coefficient"
      ) |>
      arrange(desc(`Pr(>|t|)`))
    
    removed <-
      coefficients_table %>%
      .$coefficient %>%
      .[1]
    
    if (coefficients_table$`Pr(>|t|)`[1] > cutoff) {
      controls <- setdiff(controls, removed)
    } else {
      break
    }
  }
  sort(controls)
}

significant_controls <- get_significant(unique_controls, 0.05)

reindex_data <- function(data, variables) {
  data[c(
    "country",
    "year",
    variables
  )] %>%
    filter(complete.cases(.)) |>
    select(country, year) |>
    left_join(data, by = c("country", "year")) |>
    # only create year factors for years with fully present rows
    # to main consinstency with predict
    mutate(year_factor = as.factor(year)) |>
    arrange(country, year) |>
    mutate(country_year_index = 1:n())
}

present_data <- reindex_data(default_data, c(
  "life_expectancy",
  "log_gdp_per_capita",
  "log_price_of_life",
  unique_controls
))

present_significant_data <- reindex_data(default_data, c(
  "life_expectancy",
  "log_gdp_per_capita",
  "log_price_of_life",
  significant_controls
))

controls_frame <-
  bind_cols(
    present_data[unique_controls],
    # expand year into dummies
    present_data |>
      select(year) |>
      mutate(year = as.factor(year)) |>
      model.matrix(~ year, data = _) |>
      as_tibble() |>
      select(-`(Intercept)`)
  )

indexed_price_data <-
  inner_join(
    filter_price_data(transformed_prices),
    present_data |>
      select(country, year, country_year_index),
    by = c("country", "year")
  ) |>
  arrange(country_year_index)

if (sum(is.na(indexed_price_data$country_year_index)) > 0) {
  stop("Missing countries")
}

indexed_countries <-
  present_data |>
  select(-year_factor) |>
  pivot_longer(
    c(-country, -year),
    names_to = "variable",
    values_to = "value"
  ) |>
  group_by(country) |>
  summarize(
    value = mean(value),
    .groups = "drop"
  ) |>
  arrange(country) |>
  mutate(country_index = seq_len(n()))

indexed_country_years <-
  present_data |>
  left_join(
    indexed_countries |>
      select(country, country_index),
    by = "country"
  )

life_expectancies <-
  world_bank_data |>
  filter(year == recent_year) |>
  select(life_expectancy) |>
  filter(!is.na(life_expectancy)) %>%
  .$life_expectancy

age_range <- tibble(
  life_expectancy =
    round(min(life_expectancies)):
    round(max(life_expectancies))
)

raw_map_data <-
  st_read("data/map_data.gpkg", quiet = TRUE) |>
  select(map_country = name_long, country_code_2 = iso_a2) |>
  # with no country code, we can't match
  filter(!is.na(country_code_2)) |>
  # replace country names
  left_join(country_data, by = "country_code_2")

if (nrow(raw_map_data |> filter(is.na(country))) > 0) {
  stop("Missing map countries")
}

full_data <-
  inner_join(
    world_bank_data_transformed,
    summarized_prices,
    by = c("country", "year"),
    relationship = "many-to-many"
  ) |>
  mutate(
    year_factor = as.factor(year),
    number_of_observations = coalesce(number_of_observations, 0)
  )

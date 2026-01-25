library(carData) # needed for car
library(timechange) # needed for lubridate

library(car)
library(fixest)
library(Hmisc)
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
default_gdp_deflator_kind <- "CPI medical"
default_price_exchange_kind <- "PPP GDP"
default_price_deflator_kind <- "CPI medical"
default_price_kind <- "article ratios"

example_country <- "United Kingdom"
example_developed_country <- "Finland"
example_developing_country <- "Senegal"
head_tail_size <- 3
deflator_year <- 2024
display_figures <- 3
minimum_presence <- 0
minimum_years <- 6
output_folder <- "~/Desktop"
recent_year <- 2015
year_regex <- rex(n_times(digit, 4))
primary_school_cutoff <- 45

filter_world_bank_data <- function(data,
  chosen_gdp_exchange_kind = default_gdp_exchange_kind,
  chosen_gdp_deflator_kind = default_gdp_deflator_kind
) {
  data %>%
    filter(
      gdp_exchange_kind == chosen_gdp_exchange_kind &
        gdp_deflator_kind == chosen_gdp_deflator_kind
    ) %>%
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
  data %>%
    filter(
        price_kind == chosen_price_kind &
        price_exchange_kind == chosen_price_exchange_kind &
        price_deflator_kind == chosen_price_deflator_kind
    ) %>%
    select(
      -price_kind,
      -price_exchange_kind,
      -price_deflator_kind
    )
}

make_panel <- function(data) {
  pdata.frame(data, index = c("country", "year"))
}

variable_data <-
  read_csv(
    "data/selected_world_bank_variables.csv",
    show_col_types = FALSE,
    na = ""
  )

raw_long_world_bank_data <-
  read_csv("data/world_bank_data.csv", show_col_types = FALSE, na = "") %>%
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
  ) %>%
  left_join(variable_data, by = "variable_code")

na_log <- function(variable) {
  logged <- log(variable)
  ifelse(is.infinite(logged), NA, logged)
}

# TODO: add to output

logged_controls <- c(
  "alcohol_consumption_per_capita",
  "doctors_per_thousand",
  "nurses_per_thousand",
  "population_density",
  "precipitation",
  "traffic_deaths_per_hundred_thousand"
)

logged_world_bank_data <-
  raw_long_world_bank_data %>%
  semi_join(
    tibble(variable = logged_controls),
    by = c("variable")
  ) %>%
  mutate(
    value = na_log(value),
    variable = paste0("log_", variable)
  )

# all countries from all 3 datasets
# the World Bank data includes a "euro area" country
# which I need currency data for
raw_country_data <-
  read_csv("data/countrycode_data.csv", show_col_types = FALSE, na = "") %>%
  select(
    package_country = country.name.en,
    country_code_2 = iso2c,
    package_country_code_3 = iso3c
  ) %>%
  # with no country code, we can't match
  filter(!is.na(country_code_2)) %>%
  # add in world bank countries
  full_join(
    raw_long_world_bank_data %>%
      select(
        country_code_2,
        world_bank_country_code_3,
        world_bank_country
      ) %>%
      distinct(),
    by = "country_code_2"
  ) %>%
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
  raw_country_data %>%
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
  raw_country_data %>%
  select(
    -package_country,
    -world_bank_country,
    -package_country_code_3,
    -world_bank_country_code_3
  )

write_csv(country_data, paste0(output_folder, "/country_data.csv"))

clean_health_ppps <- function(base_year, number_of_rows) {
  base_year_data <-
    paste0("data/health_", base_year, ".csv") %>%
    read_csv(n_max = number_of_rows, na = "..", show_col_types = FALSE) %>%
    select(
      -`Classification Name`,
      -`Classification Code`,
      -`Series Name`,
      -`Series Code`,
    ) %>%
    rename(
      country_code_3 = `Country Code`,
      icp_country = `Country Name`
    )
  
  old_names <- names(base_year_data)
  year_columns <- stri_detect_regex(old_names, year_regex)

  # get the year at the start
  names(base_year_data)[year_columns] <-
    old_names[year_columns] %>%
    stri_match_first_regex(year_regex)

  base_year_data %>%
    select(-icp_country) %>%
    pivot_longer(
      -country_code_3,
      names_to = "year",
      values_to = "PPP_health",
      values_drop_na = TRUE,
      names_transform = as.numeric
    ) %>%
    left_join(
      base_year_data %>%
      select(country_code_3, icp_country),
      by = "country_code_3"
    )
}

raw_ppp_health_data <-
  tibble(
    base_year = c(2005, 2011, 2017, 2021),
    number_of_rows = c(195, 222, 222, 207)
  ) %>%
  group_by(base_year) %>%
  reframe(clean_health_ppps(base_year, number_of_rows)) %>%
  mutate(
    # fix a typo in the data
    country_code_3 = ifelse(
      icp_country == "Russian Federation",
      "RUS",
      country_code_3
    )
  ) %>%
  # for a given year, use data from latest base year
  group_by(country_code_3, year) %>%
  arrange(desc(base_year)) %>%
  slice(1) %>%
  ungroup %>%
  select(-base_year) %>%
  left_join(
    country_data %>%
      select(country, country_code_3),
    by = "country_code_3"
  )

# check to make sure we aren't losing important countries
raw_ppp_health_data %>%
  filter(is.na(country)) %>%
  write_csv(paste0(output_folder, "/missing_PPP_countries.csv"))

world_bank_data <-
  bind_rows(
    raw_long_world_bank_data,
    logged_world_bank_data
  ) %>%
  select(
    country_code_2,
    year,
    variable,
    value
  ) %>%
  left_join(
    country_data %>%
      select(country, country_code_2),
    by = "country_code_2"
  ) %>%
  select(-country_code_2) %>%
  bind_rows(
    raw_ppp_health_data %>%
      select(-country_code_3, -icp_country) %>%
      filter(!is.na(country)) %>%
      rename(value = PPP_health) %>%
      mutate(variable = "PPP_health"),
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  )

deflator_data <-
  bind_rows(
    read_csv("data/GDP_deflator.csv", na = "", show_col_types = FALSE) %>%
      select(date, deflator = value) %>%
      mutate(deflator_kind = "GDP deflator"),
    read_csv("data/CPI.csv", na = "", show_col_types = FALSE) %>%
      select(date, deflator = value) %>%
      mutate(deflator_kind = "CPI"),
    read_csv("data/CPI_medical.csv", na = "", show_col_types = FALSE) %>%
      select(date, deflator = value) %>%
      mutate(deflator_kind = "CPI medical")
  ) %>%
  # average by year
  mutate(year = year(date)) %>%
  group_by(deflator_kind, year) %>%
  summarize(deflator = mean(deflator), .groups = "drop") %>%
  group_by(deflator_kind) %>%
  # USD in recent_year / basket
  # * basket / historical USD
  # = USD in recent_year / historical USD
  mutate(deflator_ratio = deflator[year == deflator_year] / deflator) %>%
  ungroup() %>%
  select(-deflator)

currency_ratios <-
  world_bank_data %>%
  select(country, year,
    `nominal exchange rate` = nominal_exchange_rate,
    `PPP GDP` = PPP_GDP,
    `PPP private consumption` = PPP_private_consumption,
    `PPP health` = PPP_health
  ) %>%
  pivot_longer(c(-country, -year),
    names_to = "exchange_rate_kind",
    # = local historical currency / historical USD
    values_to = "exchange_rate",
    values_drop_na = TRUE
  ) %>%
  # duplicate for different price adjustments
  inner_join(deflator_data, by = "year", relationship = "many-to-many") %>%
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
  vector %>%
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
  ) %>%
  rename(
    article_id = `Article ID`,
    year = `Issue Year`,
    local_price_of_life_qaly = `$/QALY from Article`,
    local_price_of_life_daly = `$/DALY from Article`,
    local_price_of_life_qaly_final = `$/QALY Final`
  ) %>%
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
  ) %>%
  rename(
    article_id = `Article ID`,
    tufts_country = `Countries`,
    tufts_currency_country = `Currency Country`,
    currency_year = `Currency Year`
  ) %>%
  distinct() %>% # remove identical rows
  group_by(article_id) %>% # use most recent entry for duplicated rows
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    # this should be ok to do because, by definition,
    # the US PPP exchange rate and nominal exchange rate are both 1
    tufts_currency_country = ifelse(
      tufts_currency_country == "International",
      "United States",
      tufts_currency_country
    ),
    currency_year = as.numeric(currency_year),
  ) %>%
  # replace countries
  left_join(country_replacements, by = "tufts_country") %>%
  mutate(country = coalesce(country_replacement, tufts_country)) %>%
  select(-country_replacement, -tufts_country) %>%
  # replace currency countries
  left_join(currency_country_replacements, by = "tufts_currency_country") %>%
  mutate(currency_country = coalesce(
    currency_country_replacement,
    tufts_currency_country
  )) %>%
  select(-tufts_currency_country, -currency_country_replacement)

# check to see if we should add more country replacements
raw_methods %>%
  select(country) %>%
  distinct() %>%
  arrange(country) %>%
  filter(!is.na(country)) %>%
  anti_join(
    country_data,
    by = "country"
  ) %>%
  write_csv(paste0(output_folder, "/unmatched_countries.csv"))

# check to see if we should add more currency country replacements
raw_methods %>%
  select(currency_country) %>%
  distinct() %>%
  arrange(currency_country) %>%
  filter(!is.na(currency_country)) %>%
  anti_join(
    country_data %>% select(currency_country = country),
    by = "currency_country"
  ) %>%
  write_csv(paste0(output_folder, "/unmatched_currency_countries.csv"))

methods <-
  raw_methods %>%
  semi_join(
    country_data %>%
      select(country),
    by = "country"
  )

expanded_prices <-
  raw_prices %>%
  select(
    ratio_id,
    `article ratios` = local_price_of_life_qaly,
    `with DALY ratios` = local_price_of_life_both,
    `with reader adjustments` = local_price_of_life_qaly_final
  ) %>%
  pivot_longer(
    -ratio_id,
    names_to = "price_kind",
    values_to = "local_price_of_life",
    values_drop_na = TRUE
  ) %>%
  left_join(
    raw_prices %>% select(article_id, ratio_id, year),
    by = "ratio_id"
  ) %>%
  select(-ratio_id)

methods_with_currency_ratios <-
  inner_join(
    methods %>%
      select(article_id, country, currency_country, currency_year),
    currency_ratios %>%
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
  ) %>%
  select(-currency_country, -currency_year)

# ratios with matching countries and currency countries
transformed_prices <-
  inner_join(
    expanded_prices,
    methods_with_currency_ratios,
    by = "article_id",
    # duplicate for deflator/exchange rate/ratio kinds
    relationship = "many-to-many"
  ) %>%
  select(-article_id) %>%
  mutate(
    # local historical currency
    # * USD in recent_year / local historical currency
    # = USD in recent_year
    log_price_of_life = log(local_price_of_life * price_currency_ratio)
  )

summarized_prices <-
  transformed_prices %>%
  group_by(
    price_kind,
    price_deflator_kind,
    price_exchange_kind,
    country,
    year
  ) %>%
  summarize(
    log_price_of_life = mean(log_price_of_life),
    number_of_observations = n(),
    .groups = "drop"
  ) %>%
  arrange(
    price_kind,
    price_deflator_kind,
    price_exchange_kind,
    country,
    year
  )

world_bank_data_transformed <-
  world_bank_data %>%
  select(
    -nominal_exchange_rate,
    -PPP_GDP,
    -PPP_private_consumption,
    -PPP_health
  ) %>%
  select(
    -alcohol_consumption_per_capita,
    -doctors_per_thousand,
    -nurses_per_thousand,
    -population_density,
    -precipitation,
    -traffic_deaths_per_hundred_thousand
  ) %>%
  inner_join(
    currency_ratios %>%
      rename(
        gdp_exchange_kind = exchange_rate_kind,
        gdp_deflator_kind = deflator_kind,
        gdp_currency_ratio = currency_ratio
      ),
    by = c("country", "year"),
    # duplicate for different price adjustments
    relationship = "many-to-many"
  ) %>%
  mutate(
    # local historical currency
    # * USD in recent_year / local historical currency
    # = USD in recent_year
    log_gdp_per_capita = log(local_GDP_per_capita * gdp_currency_ratio)
  ) %>%
  select(-gdp_currency_ratio) %>%
  arrange(gdp_exchange_kind, gdp_deflator_kind)

with_missing_outcomes <-
  filter_world_bank_data(world_bank_data_transformed) %>%
  left_join(
    filter_price_data(summarized_prices),
    by = c("country", "year")
  ) %>%
  mutate(year_factor = as.factor(year))

default_data <-
  with_missing_outcomes %>%
  filter(!is.na(log_price_of_life)) %>%
  mutate(year_factor = as.factor(year))

default_data %>%
  write_csv(paste0(output_folder, "/default_data.csv"))

all_controls <- c(
  "education_gender_parity_index",
  "GDP_growth_rate",
  "Gini_index",
  "log_alcohol_consumption_per_capita",
  "log_doctors_per_thousand",
  "log_nurses_per_thousand",
  "log_population_density",
  "log_precipitation",
  "log_traffic_deaths_per_hundred_thousand",
  "percent_basic_drinking_water",
  "percent_basic_water_sanitation",
  "percent_child_obesity",
  "percent_college_enrollment",
  "percent_electricity_access",
  "percent_female_employment",
  "percent_GDP_from_industry",
  "percent_health_spending_external",
  "percent_health_spending_out_of_pocket",
  "percent_health_spending_public",
  "percent_low_birthweight",
  "percent_open_defacation",
  "percent_primary_school_enrollment",
  "percent_primary_school_female",
  "percent_risk_medical_bankrupcy",
  "percent_safe_drinking_water",
  "percent_safe_water_sanitation",
  "percent_undernourished",
  "percent_young",
  "percent_youth_not_in_school_or_work",
  "population_growth_rate",
  "unemployment_rate"
)

if (length(symdiff(all_controls, names(default_data))) != 10) {
  stop("Controls mismatch")
}

make_plm_formula <- function(controls) {
  if (length(controls) == 0) {
    controls_string <- ""
  } else {
    controls_string <-
      controls %>%
        paste0(collapse = " + ") %>%
        paste0(" + ", .)
  }
  paste0(
    "log_price_of_life ~ life_expectancy",
    controls_string,
    " + year_factor | log_gdp_per_capita",
    controls_string,
    " + year_factor"
  ) %>%
    as.formula
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
  ) %>%
    as.formula
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
  ) %>%
    as.formula
}

make_simple_feols_formula <- function(controls) {
  controls_string <-
    c(controls, "life_expectancy") %>%
    paste0(collapse = " + ")

  paste0(
    "log_price_of_life ~ ",
    controls_string,
    " | year"
  ) %>%
    as.formula
}

# TODO: replace GDP growth rate with NY.GDP.MKTP.KD.ZG and rerun

minimally_present_variables <-
  default_data[all_controls] %>%
  sapply(function(vector) sum(!is.na(vector)) / length(vector)) %>%
  sort %>%
  .[. >= minimum_presence] %>%
  names

prioritize_most_useful <- function(controls, max_controls = length(controls)) {
  included_controls <- character(0)
  while (length(included_controls) < max_controls) {
    left_over <- setdiff(controls, included_controls)
    r_squareds <- sapply(left_over, function(control) {
      tryCatch({
        ivreg(
          make_plm_formula(c(control, included_controls)),
          data = default_data,
          weights = number_of_observations
        ) %>%
          summary() %>%
          .[["adj.r.squared"]]
      }, warning = function(w) {
        0.0
      }, error = function(e) {
        0.0
      })
    }) %>%
      sort(decreasing = TRUE)

    best_control <- names(r_squareds)[1]
    r_squared <- r_squareds[1]
    if (r_squared <= 0) {
      break
    }
    print(best_control)
    print(r_squared)
    included_controls <- c(included_controls, best_control)
  }
  included_controls
}

estimable_controls <- prioritize_most_useful(minimally_present_variables)

extra_model <- feols(
  make_feols_formula(estimable_controls),
  data = present_data,
  panel.id = c("country", "year"),
  vcov = "NW",
  weights = ~ number_of_observations
)

remove_max_vif <- function(controls, cutoff) {
  while (length(controls) > 0) {
    model <- ivreg(
      make_plm_formula(controls),
      data = default_data,
      weights = number_of_observations
    )

    vifs <- vif(model)[,"GVIF^(1/(2*Df))"]

    vifs_table <-
      tibble(
        coefficient = names(vifs),
        value = vifs
      ) %>%
      left_join(
        tibble(coefficient = controls, is_control = TRUE),
        by = "coefficient"
      ) %>%
      filter(is_control | coefficient == "life_expectancy") %>%
      arrange(desc(value))
    
    removed <- 
      vifs_table %>%
        filter(is_control) %>%
        .$coefficient %>%
        .[1]
    
    if (vifs_table$value[1] > cutoff) {
      print(removed)
      controls <- setdiff(controls, removed)
    } else {
      break
    }
  }
  sort(controls)
}

unique_controls <- remove_max_vif(estimable_controls, sqrt(10))

format_regressors <- function(data) {
  data %>%
    stri_replace_all_fixed("_", " ") %>%
    stri_replace_all_fixed("percent", "%")
}

# TODO: add to output
controls_table <-
  tibble(control = all_controls) %>%
  left_join(
    tibble(
      control = estimable_controls,
      estimable = TRUE
    ),
    by = "control"
  ) %>%
  left_join(
    tibble(
      control = unique_controls,
      collinear = FALSE
    ),
    by = "control"
  ) %>%
  anti_join(tibble(control = logged_controls), by = "control") %>%
  mutate(
    estimable = coalesce(estimable, FALSE),
    collinear = coalesce(collinear, TRUE),
    status = ifelse(
      estimable,
      ifelse(collinear,
        "Multicollinear",
        "Kept"
      ),
      "Unestimable"
    )
  ) %>%
  arrange(status, control) %>%
  select(-estimable, -collinear) %>%
  rename(Control = control, Status = status) %>%
  mutate(Control = format_regressors(Control))

get_present_data_for <- function(data, regressors) {
  data[c("country", "year", regressors)] %>%
    filter(complete.cases(.)) %>%
    select(country, year) %>%
    left_join(data, by = c("country", "year")) %>%
    # only create year factors for years with fully present rows
    # to main consinstency with predict
    mutate(year_factor = as.factor(year))
}

present_data <- get_present_data_for(
  default_data,
  c(
    "life_expectancy",
    "log_gdp_per_capita",
    "log_price_of_life",
    unique_controls
  )
)

pooled_model <- feols(
  make_feols_formula(unique_controls),
  data = present_data,
  panel.id = c("country", "year"),
  vcov = "NW",
  weights = ~ number_of_observations
)

summary(pooled_model)

get_confidence_intervals <- function(model, alpha = 0.05) {
  model_summary <- summary(model, vcov = vcovHC)
  critical_z <- qnorm(1 - alpha / 2)
  coefficients_table <- model_summary$coefficients

  if (!("z-value" %in% colnames(coefficients_table))) {
    stop("Requires a z distribution")
  }

  coefficients_table %>%
    as_tibble(rownames = "coefficient") %>%
    mutate(
      lower_bound = Estimate - critical_z * `Std. Error`,
      upper_bound = Estimate + critical_z * `Std. Error`
    )
}

get_significant_controls <- function(controls, cutoff = 0.05) {
  while (length(controls) > 0) {
    model <- feols(
      make_feols_formula(unique_controls),
      data = present_data,
      panel.id = c("country", "year"),
      vcov = "NW",
      weights = ~ number_of_observations
    )

    coefficients_table <-
      summary(model)$coeftable %>%
      as_tibble(
        rownames = "coefficient"
      ) %>%
      arrange(desc(`Pr(>|t|)`)) %>%
      semi_join(
        tibble(coefficient = controls),
        by = "coefficient"
      )
    
    removed <- 
      coefficients_table$coefficient[1]
    
    if (coefficients_table$`Pr(>|t|)`[1] > cutoff) {
      print(removed)
      controls <- setdiff(controls, removed)
    } else {
      break
    }
  }
  sort(controls)
}

significant_controls <- get_significant_controls(unique_controls)

significant_model <- feols(
  make_feols_formula(significant_controls),
  data = present_data,
  panel.id = c("country", "year"),
  vcov = "NW",
  weights = ~ number_of_observations
)

recent_predictions <-
  with_missing_outcomes %>%
  select(-year_factor) %>%
  left_join(
    present_data %>%
      # need to use the same factors for consistency
      select(year, year_factor) %>%
      distinct,
    by = "year"
  ) %>%
  arrange(country, year) %>%
  mutate(
    prediction = predict(pooled_model, newdata = .),
  ) %>%
  filter(
    year == recent_year &
      !is.na(prediction)
  ) %>%
  mutate(price_of_life = exp(prediction)) %>%
  arrange(desc(price_of_life)) %>%
  mutate(rank = seq_len(n()))

write_csv(
  recent_predictions,
  paste0(output_folder, "/recent_predictions.csv")
)

example_prediction <-
  recent_predictions %>%
  filter(country == example_country)

best_prediction <- recent_predictions %>% slice(1)
worst_prediction <- recent_predictions %>% slice(nrow(.))

model_coefficients <- coef(pooled_model)
life_expectancy_coefficient <- model_coefficients[["fit_life_expectancy"]]

life_expectancies <-
  world_bank_data %>%
  filter(year == recent_year) %>%
  select(life_expectancy) %>%
  filter(!is.na(life_expectancy)) %>%
  .$life_expectancy

age_range <- tibble(
  life_expectancy =
    round(min(life_expectancies)):
    round(max(life_expectancies))
)

intersection_data <-
  bind_rows(
    age_range %>%
      mutate(
        price_of_life = exp(
          example_prediction$prediction + (
            life_expectancy - example_prediction$life_expectancy
          ) * life_expectancy_coefficient
        ),
        side = "Marginal cost of life"
      ),
    age_range %>%
      mutate(
        price_of_life = exp(example_prediction$prediction),
        side = "Marginal benefit of life"
      )
  ) %>%
  mutate(price_of_life_in_thousands = price_of_life / 1000) %>%
  rename(
    `Life expectancy` = life_expectancy,
    `Price of life, in thousand International $ / QALY` =
    price_of_life_in_thousands,
    Side = side
  )

bold_header <- function(table) {
  table %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling()
}

rank_table <-
  bind_rows(
    recent_predictions %>% slice_head(n = head_tail_size),
    recent_predictions %>% slice_tail(n = head_tail_size)
  ) %>%
  mutate(
    price_of_life =
    signif(price_of_life, display_figures) %>%
    format(
      big.mark = ",",
      drop0trailing = TRUE,
      scientific = FALSE,
      trim = TRUE
    ) %>%
    paste0("$", .)
  ) %>%
  select(
    Rank = rank,
    Country = country,
    `Marginal cost of life` = price_of_life
  )

raw_map_data <-
  st_read("data/map_data.gpkg", quiet = TRUE) %>%
  select(map_country = name_long, country_code_2 = iso_a2) %>%
  # with no country code, we can't match
  filter(!is.na(country_code_2)) %>%
  # replace country names
  left_join(country_data, by = "country_code_2")

if (nrow(raw_map_data %>% filter(is.na(country))) > 0) {
  stop("Missing map countries")
}

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

plot(map, border = NA, key.pos = 1, main = NULL, logz = TRUE)

# TODO: add to output
logged_variable_table <-
  summarized_prices %>%
  filter_price_data %>%
  select(country, year) %>%
  distinct() %>%
  left_join(world_bank_data, by = c("country", "year")) %>%
  pivot_longer(
    c(-year, -country),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  summarize(
    number_positive = sum(value > 0, na.rm = TRUE),
    number_non_positive = sum(value <= 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  semi_join(
    tibble(variable = logged_controls),
    by = "variable"
  ) %>%
  mutate(variable = format_regressors(variable)) %>%
  select(
    Variable = variable,
    `Number positive` = number_positive,
    `Number non-positive` = number_non_positive
  )

variable_table <-
  summarized_prices %>%
  filter_price_data %>%
  select(country, year) %>%
  distinct() %>%
  left_join(world_bank_data, by = c("country", "year")) %>%
  pivot_longer(
    c(-year, -country),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  summarize(
    percent_positive =
      (sum(value > 0, na.rm = TRUE) / sum(!is.na(value)) * 100) %>%
      signif(display_figures) %>%
      paste0("%"),
    percent_missing =
    (sum(is.na(value)) / n() * 100) %>%
    signif(display_figures) %>%
    paste0("%"),
    .groups = "drop"
  ) %>%
  left_join(variable_data, by = "variable") %>%
  mutate(variable = stri_replace_all_fixed(variable, "_", " ")) %>%
  select(
    Variable = variable,
    Code = variable_code,
    `% missing` = percent_missing,
    `% positive` = percent_positive
  )

pooled_summary <- summary(pooled_model)

# TODO: add to output
extras <-
  c(
    `RMSE` = stats$rmse,
    `Adjusted R squared` = stats$ar2,
    `F-test p-value` = stats$ivf1$p,
    `Wu-Hausman p-value` = stats$wh$p
  )

coefficients_table <-
  pooled_model %>%
  summary %>%
  .$coeftable %>%
  as_tibble(rownames = "regressor") %>%
  rename(
    `Standard error` = `Std. Error`,
    `p-value` = `Pr(>|t|)`
  ) %>%
  rowwise() %>%
  mutate(
    regressor = ifelse(
      regressor == "(Intercept)",
      "intercept",
      regressor
    ) %>%
      stri_replace_all_fixed("year_factor", "year=") %>%
      format_regressors,
    Estimate = signif(Estimate, display_figures),
    `p-value` = signif(`p-value`, display_figures) %>% format()
  ) %>%
  ungroup() %>%
  select(Regressor = regressor, Estimate, `Standard error`, `p-value`)

get_plm_interval <- function(model, parameter = "life_expectancy") {
  confidence_intervals <-
    get_confidence_intervals(model) %>%
    filter(coefficient == parameter)

  tibble(
    `2.5%` =
      confidence_intervals$lower_bound %>%
      signif(display_figures),
    `97.5%` =
      confidence_intervals$upper_bound %>%
      signif(display_figures)
  )
}

get_feols_interval <- function(model, parameter = "fit_life_expectancy") {
  confidence_intervals <-
    confint(model) %>%
    as_tibble(rownames = "regressor") %>%
    filter(regressor == parameter)
  
  tibble(
    `2.5%` =
      confidence_intervals$`2.5 %` %>%
      signif(display_figures),
    `97.5%` =
      confidence_intervals$`97.5 %` %>%
      signif(display_figures)
  )
}

model_without_endogeneity <- feols(
  make_simple_feols_formula(unique_controls),
  data = default_data,
  panel.id = c("country", "year"),
  vcov = "NW",
  weights = ~ number_of_observations
)

fixed_effects_model <- feols(
  make_fixed_effects_formula(unique_controls),
  data = present_data,
  panel.id = c("country", "year"),
  vcov = "NW",
  weights = ~ number_of_observations
)

multicollinear_model <- feols(
  make_feols_formula(estimable_controls),
  data = present_data,
  panel.id = c("country", "year"),
  vcov = "NW",
  weights = ~ number_of_observations
)

specification_robustness_table <-
  bind_rows(
    get_feols_interval(pooled_model) %>%
      mutate(Specification = "base"),
    get_feols_interval(multicollinear_model) %>%
      mutate(Specification = "with multicollinear controls"),
    get_feols_interval(fixed_effects_model) %>%
      mutate(Specification = "fixed effects"),
    get_feols_interval(
      model_without_endogeneity,
      parameter = "life_expectancy"
    ) %>%
      mutate(Specification = "without endogeneity")
  )

get_data_coefficient <- function(data) {
  tryCatch(
    {
      plm(
        make_plm_formula(unique_controls),
        data = make_panel(data),
        model = "random"
      ) %>%
        get_plm_interval
    },
    warning = function(w) {
      tibble(
        `2.5%` = NA,
        `97.5%` = NA
      )
    },
    error = function(e) {
      tibble(
        `2.5%` = NA,
        `97.5%` = NA
      )
    }
  )
}

full_data <-
  inner_join(
    world_bank_data_transformed,
    summarized_prices,
    by = c("country", "year"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    year_factor = as.factor(year),
    number_of_observations = coalesce(number_of_observations, 0)
  )

robustness_table <-
  full_data %>%
  nest_by(
    price_kind,
    price_deflator_kind,
    price_exchange_kind,
    gdp_exchange_kind,
    gdp_deflator_kind,
    .key = "data"
  ) %>%
  mutate(
    data = get_data_coefficient(data)
  ) %>%
  unnest(data) %>%
  ungroup

gdp_deflators_robustness_table <-
  robustness_table %>%
  filter(
    price_kind == default_price_kind &
      price_deflator_kind == default_price_deflator_kind &
      price_exchange_kind == default_price_exchange_kind &
      gdp_exchange_kind == default_gdp_exchange_kind
  ) %>%
  select(
    -price_kind,
    -price_deflator_kind,
    -price_exchange_kind,
    -gdp_exchange_kind
  ) %>%
  rename(`GDP deflators` = gdp_deflator_kind)

gdp_exchange_robustness_table <-
  robustness_table %>%
  filter(
    price_kind == default_price_kind &
      price_deflator_kind == default_price_deflator_kind &
      price_exchange_kind == default_price_exchange_kind &
      gdp_deflator_kind == default_gdp_deflator_kind
  ) %>%
  select(
    -price_kind,
    -price_deflator_kind,
    -price_exchange_kind,
    -gdp_deflator_kind
  ) %>%
  rename(`GDP exchange rates` = gdp_exchange_kind)

prices_robustness_table <-
  robustness_table %>%
  filter(
    price_deflator_kind == default_price_deflator_kind &
      price_exchange_kind == default_price_exchange_kind &
      gdp_exchange_kind == default_gdp_exchange_kind &
      gdp_deflator_kind == default_gdp_deflator_kind
  ) %>%
  select(
    -price_deflator_kind,
    -price_exchange_kind,
    -gdp_deflator_kind,
    -gdp_exchange_kind
  ) %>%
  rename(Prices = price_kind)

price_deflators_table <-
  robustness_table %>%
  filter(
    price_kind == default_price_kind &
      price_exchange_kind == default_price_exchange_kind &
      gdp_exchange_kind == default_gdp_exchange_kind &
      gdp_deflator_kind == default_gdp_deflator_kind
  ) %>%
  select(
    -price_kind,
    -price_exchange_kind,
    -gdp_deflator_kind,
    -gdp_exchange_kind
  ) %>%
  rename(`Price deflators` = price_deflator_kind)

price_exchange_table <-
  robustness_table %>%
  filter(
    price_kind == default_price_kind &
      price_deflator_kind == default_price_deflator_kind &
      gdp_exchange_kind == default_gdp_exchange_kind &
      gdp_deflator_kind == default_gdp_deflator_kind
  ) %>%
  select(
    -price_kind,
    -price_deflator_kind,
    -gdp_deflator_kind,
    -gdp_exchange_kind
  ) %>%
  rename(`Price exchange rates` = price_exchange_kind)


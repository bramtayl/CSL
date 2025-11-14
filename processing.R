library(knitr)
# needed for lubridate
library(timechange)
library(dplyr, warn.conflicts = FALSE)
library(fixest)
library(ivreg)
library(lubridate, warn.conflicts = FALSE)
library(plm, warn.conflicts = FALSE)
library(purrr)
library(readr)
library(rnaturalearth)
suppressPackageStartupMessages(library(sf))
library(stringi)
library(tidyr)

example_country <- "United Kingdom"
head_tail_size <- 3
deflator_year <- 2024
display_figures <- 3
minimum_years <- 6
output_folder <- "~/Desktop"
recent_year <- 2020

# replace world bank country names with our country names
raw_world_bank_data <-
  read_csv("data/world_bank_data.csv", show_col_types = FALSE, na = "") %>%
  select(
    # 2 digit country codes
    country_code_2 = iso2c,
    # 3 digit world bank codes
    world_bank_country_code_3 = iso3c,
    world_bank_country = country,
    year = date,
    life_expectancy = SP.DYN.LE00.IN,
    local_GDP_per_capita = NY.GDP.PCAP.CN,
    nominal_exchange_rate = PA.NUS.FCRF,
    percent_health_spending_public = SH.XPD.GHED.CH.ZS,
    population_density = EN.POP.DNST,
    PPP_GDP = PA.NUS.PPP,
    PPP_private_consumption = PA.NUS.PRVT.PP
  ) %>%
  # with no country code, we can't match
  filter(!is.na(country_code_2))

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
    raw_world_bank_data %>%
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
        !is.na(package_country_code_3)
    ) %>%
    filter(
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
      -`Country Name`,
      -`Classification Name`,
      -`Classification Code`,
      -`Series Name`,
      -`Series Code`,
    ) %>%
    rename(country_code_3 = `Country Code`) %>%
    mutate(
      # fix a typo in the 2021 data (ugh...)
      country_code_3 = ifelse(
        country_code_3 == "RUT",
        "RUS",
        country_code_3
      )
    )

  year_columns <- 2:length(base_year_data)

  # get the year at the start
  names(base_year_data)[year_columns] <-
    names(base_year_data)[year_columns] %>%
    stri_sub(1, 4)

  base_year_data %>%
    pivot_longer(
      -country_code_3,
      names_to = "year",
      values_to = "PPP_health",
      values_drop_na = TRUE
    ) %>%
    mutate(year = as.numeric(year))
}

raw_ppp_health_data <-
  tibble(
    base_year = c(2005, 2011, 2017, 2021),
    number_of_rows = c(195, 222, 222, 207)
  ) %>%
  group_by(base_year) %>%
  reframe(clean_health_ppps(base_year, number_of_rows)) %>%
  ungroup() %>%
  # use later base year for these two years
  filter(
    !(base_year == 2011 & year == 2011) &
      !(base_year == 2017 & year == 2017)
  ) %>%
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
  full_join(
    raw_world_bank_data %>%
      select(-world_bank_country, -world_bank_country_code_3) %>%
      left_join(
        country_data %>%
          select(country, country_code_2),
        by = "country_code_2"
      ) %>%
      select(-country_code_2),
    raw_ppp_health_data %>%
      select(-country_code_3) %>%
      filter(!is.na(country)),
    by = c("country", "year")
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

replace_0s <- function(vector) {
  vector %>%
    as.numeric() %>%
    # must be positive
    ifelse(. <= 0, NA, .)
}

raw_ratios <-
  read_csv(
    "data/ratios.csv",
    show_col_types = FALSE,
    col_types = cols_only(
      `Article ID` = col_character(),
      `Issue Year` = col_double(),
      `$/QALY from Article` = col_character(),
      `$/DALY from Article` = col_character(),
      `$/QALY Final` = col_character(),
      `$/DALY Final` = col_character(),
      `$/QALY Final in Current USD` = col_character(),
      `$/DALY Final in Current USD` = col_character()
    )
  ) %>%
  rename(
    article_id = `Article ID`,
    year = `Issue Year`,
    local_price_of_life_qaly = `$/QALY from Article`,
    local_price_of_life_daly = `$/DALY from Article`,
    local_price_of_life_qaly_final = `$/QALY Final`,
    local_price_of_life_daly_final = `$/DALY Final`,
    adjusted_price_of_life_qaly = `$/QALY Final in Current USD`,
    adjusted_price_of_life_daly = `$/DALY Final in Current USD`
  ) %>%
  left_join(
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
      ungroup(),
    by = "article_id"
  ) %>%
  select(-article_id) %>%
  mutate(
    local_price_of_life_qaly = replace_0s(local_price_of_life_qaly),
    local_price_of_life_daly = replace_0s(local_price_of_life_daly),
    local_price_of_life_qaly_final = replace_0s(local_price_of_life_qaly_final),
    local_price_of_life_daly_final = replace_0s(local_price_of_life_daly_final),
    adjusted_price_of_life_qaly = replace_0s(adjusted_price_of_life_qaly),
    adjusted_price_of_life_daly = replace_0s(adjusted_price_of_life_daly),
    local_price_of_life = coalesce(
      local_price_of_life_qaly,
      local_price_of_life_daly
    ),
    local_price_of_life_final = coalesce(
      local_price_of_life_qaly_final,
      local_price_of_life_daly_final
    ),
    adjusted_price_of_life = coalesce(
      adjusted_price_of_life_qaly,
      adjusted_price_of_life_daly
    ),
    # 0 means NA
    year = as.numeric(year) %>% ifelse(. == 0, NA, .),
    currency_year = as.numeric(currency_year),
    # this should be ok to do because, by definition,
    # the US PPP exchange rate and nominal exchange rate are both 1
    # ideally, we could undo any conversions done by authors
    # but we don't know what the original currencies were
    tufts_currency_country = ifelse(
      tufts_currency_country == "International",
      "United States",
      tufts_currency_country
    ),
    ratio_id = seq_len(n())
  )

tufts_meta <-
  raw_ratios %>%
  select(
    ratio_id,
    year,
    tufts_country,
    tufts_currency_country,
    currency_year
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
tufts_meta %>%
  select(country) %>%
  distinct() %>%
  arrange(country) %>%
  anti_join(
    country_data %>% select(country),
    by = "country"
  ) %>%
  write_csv(paste0(output_folder, "/unmatched_countries.csv"))

# check to see if we should add more currency country replacements
tufts_meta %>%
  select(currency_country) %>%
  distinct() %>%
  arrange(currency_country) %>%
  filter(!is.na(currency_country)) %>%
  anti_join(
    country_data %>% select(currency_country = country),
    by = "currency_country"
  ) %>%
  write_csv(paste0(output_folder, "/unmatched_currency_countries.csv"))

ratios <-
  raw_ratios %>%
  select(
    ratio_id,
    `article ratios` = local_price_of_life_qaly,
    `with DALY ratios` = local_price_of_life,
    `with reader adjustments` = local_price_of_life_qaly_final,
    `with Tufts currency adjustments` = adjusted_price_of_life_qaly
  ) %>%
  pivot_longer(
    -ratio_id,
    names_to = "ratios_kind",
    values_to = "local_price_of_life",
    values_drop_na = TRUE
  ) %>%
  left_join(
    tufts_meta,
    by = "ratio_id"
  )

# ratios with matching countries and currency countries
matched_ratios <-
  bind_rows(
    ratios %>%
      filter(ratios_kind != "with Tufts currency adjustments") %>%
      semi_join(
        country_data %>% select(currency_country = country),
        by = "currency_country"
      ) %>%
      filter(!is.na(currency_year)) %>%
      inner_join(
        currency_ratios %>%
          rename(
            currency_country = country,
            currency_year = year,
            ratio_deflator_kind = deflator_kind,
            ratio_exchange_rate_kind = exchange_rate_kind,
            price_currency_ratio = currency_ratio
          ),
        by = c("currency_year", "currency_country"),
        # duplicate for different price adjustments
        relationship = "many-to-many"
      ),
    ratios %>%
      filter(ratios_kind == "with Tufts currency adjustments") %>%
      mutate(
        ratio_exchange_rate_kind = "N/A",
        ratio_deflator_kind = "N/A",
        price_currency_ratio = 1,
        currency_country = "United States",
        currency_year = deflator_year
      )
  ) %>%
  semi_join(
    country_data %>%
      select(country),
    by = "country"
  ) %>%
  mutate(
    log_local_price_of_life = log(local_price_of_life),
    # local historical currency
    # * USD in recent_year / local historical currency
    # = USD in recent_year
    log_price_of_life = log_local_price_of_life + log(price_currency_ratio)
  ) %>%
  select(-exchange_rate, -deflator_ratio, -price_currency_ratio)

world_bank_data_transformed <-
  world_bank_data %>%
  select(
    -nominal_exchange_rate,
    -PPP_GDP,
    -PPP_private_consumption,
    -PPP_health
  ) %>%
  filter(
    !is.na(life_expectancy) &
      !is.na(local_GDP_per_capita) &
      !is.na(percent_health_spending_public) &
      !is.na(population_density)
  ) %>%
  mutate(
    log_population_density = log(population_density)
  ) %>%
  select(-population_density) %>%
  inner_join(
    currency_ratios %>%
      rename(
        GDP_exchange_rate_kind = exchange_rate_kind,
        GDP_deflator_kind = deflator_kind,
        GDP_currency_ratio = currency_ratio
      ),
    by = c("country", "year"),
    # duplicate for different price adjustments
    relationship = "many-to-many"
  ) %>%
  mutate(
    log_local_gdp_per_capita = log(local_GDP_per_capita),
    # local historical currency
    # * USD in recent_year / local historical currency
    # = USD in recent_year
    log_gdp_per_capita = log(local_GDP_per_capita * GDP_currency_ratio)
  ) %>%
  select(-GDP_currency_ratio)

formula <-
  log_price_of_life ~
  # exogenous
  log_population_density +
  percent_health_spending_public +
  year +
  # endogenous
  life_expectancy |
  # exogenous
  log_population_density +
  percent_health_spending_public +
  year +
  # instruments
  log_gdp_per_capita

pooled_data <-
  inner_join(
    matched_ratios %>%
      filter(
        ratios_kind == "article ratios" &
          ratio_exchange_rate_kind == "PPP GDP" &
          ratio_deflator_kind == "CPI medical"
      ) %>%
      select(-ratios_kind, -ratio_exchange_rate_kind, -ratio_deflator_kind),
    world_bank_data_transformed %>%
      filter(
        GDP_exchange_rate_kind == "PPP GDP" &
          GDP_deflator_kind == "CPI medical"
      ) %>%
      select(-GDP_exchange_rate_kind, -GDP_deflator_kind),
    by = c("country", "year")
  )

non_linear_adjustments_pooled <-
  pooled_data %>%
  mutate(
    log_deflator_ratio = log(deflator_ratio),
    log_exchange_rate = log(exchange_rate)
  ) %>%
  ivreg(
    log_local_price_of_life ~
      # exogenous
      log_population_density +
        percent_health_spending_public +
        year +
        log_deflator_ratio +
        log_exchange_rate +
        # endogenous
        life_expectancy |
        # exogenous
        log_population_density +
          percent_health_spending_public +
          year +
          log_deflator_ratio +
          log_exchange_rate +
          # instruments
          log_local_gdp_per_capita,
    data = .
  )

by_country_and_year <-
  matched_ratios %>%
  group_by(
    ratios_kind,
    ratio_exchange_rate_kind,
    ratio_deflator_kind,
    country, year
  ) %>%
  summarize(
    log_price_of_life = mean(log_price_of_life),
    number_of_observations = n(),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  inner_join(
    world_bank_data_transformed,
    by = c("country", "year"),
    relationship = "many-to-many"
  )

by_country_and_year %>%
  write_csv(paste0(output_folder, "/by_country_and_year.csv"))

by_country <-
  by_country_and_year %>%
  group_by(
    ratios_kind,
    ratio_deflator_kind,
    ratio_exchange_rate_kind,
    GDP_exchange_rate_kind,
    GDP_deflator_kind,
    country
  ) %>%
  summarize(
    log_price_of_life = weighted.mean(
      log_price_of_life,
      number_of_observations
    ),
    year = weighted.mean(year, number_of_observations),
    life_expectancy = weighted.mean(life_expectancy, number_of_observations),
    log_gdp_per_capita = weighted.mean(
      log_gdp_per_capita,
      number_of_observations
    ),
    log_population_density = weighted.mean(
      log_population_density,
      number_of_observations
    ),
    percent_health_spending_public = weighted.mean(
      percent_health_spending_public,
      number_of_observations
    ),
    number_of_observations = sum(number_of_observations),
    .groups = "drop"
  ) %>%
  arrange(country)

main_data <-
  by_country %>%
  filter(
    ratios_kind == "article ratios" &
      GDP_exchange_rate_kind == "PPP GDP" &
      GDP_deflator_kind == "CPI medical" &
      ratio_exchange_rate_kind == "PPP GDP" &
      ratio_deflator_kind == "CPI medical"
  ) %>%
  select(
    -ratios_kind,
    -GDP_exchange_rate_kind,
    -GDP_deflator_kind,
    -ratio_exchange_rate_kind,
    -ratio_deflator_kind,
  )

model <-
  ivreg(
    formula,
    data = main_data,
    weights = number_of_observations
  )

model_summary <- summary(model)

simple_summary <-
  ivreg(
    log_price_of_life ~
      # exogenous
      log_population_density +
        percent_health_spending_public +
        year +
        # endogenous
        life_expectancy,
    data = main_data,
    weights = number_of_observations
  ) %>%
  summary

recent_predictions <-
  world_bank_data_transformed %>%
  filter(
    year == recent_year &
      GDP_exchange_rate_kind == "PPP GDP" &
      GDP_deflator_kind == "CPI medical"
  ) %>%
  mutate(
    prediction = predict(model, newdata = .),
    price_of_life = exp(prediction)
  ) %>%
  arrange(desc(prediction))

example_prediction <-
  recent_predictions %>%
  filter(country == example_country)

best_prediction <- recent_predictions %>% slice(1)
worst_prediction <- recent_predictions %>% slice(nrow(.))

life_expectancy_coefficient <- coef(model)[["life_expectancy"]]

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

get_head_and_tail <- function(data) {
  with_rank <- data %>% mutate(rank = seq_len(n()))
  bind_rows(
    with_rank %>% slice_head(n = head_tail_size),
    with_rank %>% slice_tail(n = head_tail_size)
  )
}

confidence_intervals <- confint(model)
model_coefficients <- coef(model)

panel_data <-
  by_country_and_year %>%
  filter(
    ratios_kind == "article ratios" &
      GDP_exchange_rate_kind == "PPP GDP" &
      GDP_deflator_kind == "CPI medical" &
      ratio_exchange_rate_kind == "PPP GDP" &
      ratio_deflator_kind == "CPI medical"
  ) %>%
  mutate(year_index = year)

get_panel_residuals <- function(method) {
  panel_data %>%
  group_by(country) %>%
  # without enough years the stationarity test will fail
  filter(n() >= minimum_years) %>%
  ungroup() %>%
  mutate(year_index = year) %>%
  pdata.frame(index = c("country", "year_index")) %>%
  plm(formula, data = ., model = method) %>%
  summary(.) %>%
  .$resid
}

test_stationarity <- function(data, test, pmax = 1, exo = "none") {
  statistic <- purtest(data, pmax = pmax, exo = exo, test = test)$statistic
  tibble(
    Method = statistic$method,
    `p-value` = statistic$p.value
  )
}

test_stationarity_methods <- function(model_code) {
  residuals <- 
    panel_data %>%
    group_by(country) %>%
    # without enough years the stationarity test will fail
    filter(n() >= minimum_years) %>%
    ungroup() %>%
    mutate(year_index = year) %>%
    pdata.frame(index = c("country", "year_index")) %>%
    plm(formula, data = ., model = model_code) %>%
    summary(.) %>%
    .$resid

  # fd method seems to drop pseries attributes
  # so I need to reconstruct it
  residuals_pseries <-
    tibble(
      value = residuals,
      country_year = names(residuals)
    ) %>%
    separate(
      country_year, into = c("country", "year"), sep = "-"
    ) %>%
    pdata.frame(index = c("country", "year")) %>%
    pseriesfy()

  map(
    c("madwu", "invnormal", "logit"),
    function(test) {
      test_stationarity(residuals_pseries$value, test)
    }
  ) %>%
  list_rbind %>%
  mutate(
    `p-value` = signif(`p-value`, display_figures) %>% format(),
    Method = stri_match_first_regex(Method, "^(.*)\\s\\(.*\\)")[, 2]
  ) %>%
  select(Method, `p-value`)
}

stationarity_table <-
  tibble(
    model_code = c("within", "random", "pooling", "fd"),
    Model = c(
      "fixed effects",
      "random effects",
      "pooling",
      "first differencing"
    )
  ) %>%
  group_by(Model) %>%
  reframe(test_stationarity_methods(model_code))

bold_header <- function(table) {
  table %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling()
}

rank_table <-
  recent_predictions %>%
  get_head_and_tail() %>%
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

variable_table <-
  matched_ratios %>%
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
    percent_missing =
    (sum(is.na(value)) / n() * 100) %>%
    signif(display_figures) %>%
    paste0("%"),
    .groups = "drop"
  ) %>%
  left_join(
    read_csv(
      "data/selected_world_bank_variables.csv",
      show_col_types = FALSE,
      na = ""
    ),
    by = "variable"
  ) %>%
  mutate(variable = stri_replace_all_fixed(variable, "_", " ")) %>%
  select(
    Variable = variable,
    Code = variable_code,
    `% missing` = percent_missing
  )

coefficients_table <- tibble(
  raw_regressor = c(
    "(Intercept)",
    "log_population_density",
    "percent_health_spending_public",
    "year",
    "life_expectancy"
  ),
  Regressor = c(
    "Intercept",
    "log population density",
    "percent health spending public",
    "year",
    "life expectancy"
  )
) %>%
  left_join(
    model_summary$coefficients %>%
      as_tibble(rownames = "raw_regressor"),
    by = "raw_regressor"
  ) %>%
  select(-raw_regressor) %>%
  rename(`p-value` = `Pr(>|t|)`) %>%
  rowwise() %>%
  mutate(
    Estimate = signif(Estimate, display_figures),
    `p-value` = signif(`p-value`, display_figures) %>% format()
  ) %>%
  ungroup() %>%
  select(Regressor, Estimate, `p-value`)

diagnostics_table <-
  bind_rows(
    tibble(
      Diagnostic = "Wald test",
      `p-value` = model_summary$waldtest[[2]]
    ),
    model_summary$diagnostics %>%
      as_tibble(rownames = "Diagnostic") %>%
      filter(Diagnostic != "Sargan") %>%
      select(Diagnostic, `p-value`)
  ) %>%
  mutate(`p-value` = signif(`p-value`, display_figures))

get_interval <- function(model, parameter = "life_expectancy") {
  confidence_intervals <- confint(model)
  tibble(
    `2.5%` =
      confidence_intervals[parameter, "2.5 %"] %>%
      signif(display_figures),
    `97.5%` =
      confidence_intervals[parameter, "97.5 %"] %>%
      signif(display_figures)
  )
}

get_iv_summary <- function(data, groups) {
  confidence_intervals <- confint(ivreg(
    formula,
    data = data,
    weights = number_of_observations
  ))
  tibble(
    `2.5%` =
      confidence_intervals["life_expectancy", "2.5 %"] %>%
      signif(display_figures),
    `97.5%` =
      confidence_intervals["life_expectancy", "97.5 %"] %>%
      signif(display_figures)
  )
}

specification_robustness_table <-
  bind_rows(
    model %>%
      get_interval %>%
      mutate(Specification = "country averages"),
    feols(
      log_price_of_life ~
        # exogenous
        log_population_density +
          percent_health_spending_public | year |
          life_expectancy ~
        # instruments
        log_gdp_per_capita,
      data = pooled_data,
      cluster = ~country + year
    ) %>%
      get_interval(parameter = "fit_life_expectancy") %>%
      mutate(Specification = "pooled"),
    feols(
      log_local_price_of_life ~
        # exogenous
        log_population_density +
          percent_health_spending_public +
          log(exchange_rate) | year |
          # exogenous
          life_expectancy ~
        # instruments
        log_gdp_per_capita,
      data = pooled_data,
      cluster = ~country + year
    ) %>%
      get_interval(parameter = "fit_life_expectancy") %>%
      mutate(Specification = "non-linear currency adjustments"),
    ivreg(
      log_price_of_life ~
        # exogenous
        log_population_density +
          percent_health_spending_public +
          year +
          # endogenous
          life_expectancy,
      data = main_data,
      weights = number_of_observations
    ) %>%
      get_interval %>%
      mutate(Specification = "without endogeneity")
  )

robustness_table <-
  by_country %>%
  group_by(
    ratios_kind,
    ratio_deflator_kind,
    ratio_exchange_rate_kind,
    GDP_exchange_rate_kind,
    GDP_deflator_kind
  ) %>%
  group_modify(get_iv_summary) %>%
  ungroup()

ratios_robustness_table <-
  robustness_table %>%
  filter(
    (ratio_deflator_kind == "CPI medical" | ratio_deflator_kind == "N/A") &
      (
        ratio_exchange_rate_kind == "PPP GDP" |
          ratio_exchange_rate_kind == "N/A"
      ) &
      (GDP_deflator_kind == "CPI medical") &
      (GDP_exchange_rate_kind == "PPP GDP")
  ) %>%
  select(
    -ratio_deflator_kind,
    -ratio_exchange_rate_kind,
    -GDP_deflator_kind,
    -GDP_exchange_rate_kind
  ) %>%
  rename(Ratios = ratios_kind)

gdp_deflators_robustness_table <-
  robustness_table %>%
  filter(
    ratios_kind == "article ratios" &
      (ratio_deflator_kind == "CPI medical" | ratio_deflator_kind == "N/A") &
      (
        ratio_exchange_rate_kind == "PPP GDP" |
          ratio_exchange_rate_kind == "N/A"
      ) &
      (GDP_exchange_rate_kind == "PPP GDP")
  ) %>%
  select(
    -ratios_kind,
    -ratio_deflator_kind,
    -ratio_exchange_rate_kind,
    -GDP_exchange_rate_kind
  ) %>%
  rename(`GDP deflators` = GDP_deflator_kind)

gdp_exchange_robustness_table <-
  robustness_table %>%
  filter(
    ratios_kind == "article ratios" &
      (ratio_deflator_kind == "CPI medical" | ratio_deflator_kind == "N/A") &
      (
        ratio_exchange_rate_kind == "PPP GDP" |
          ratio_exchange_rate_kind == "N/A"
      ) &
      (GDP_deflator_kind == "CPI medical")
  ) %>%
  select(
    -ratios_kind,
    -ratio_deflator_kind,
    -ratio_exchange_rate_kind,
    -GDP_deflator_kind
  ) %>%
  rename(`GDP exchange rates` = GDP_exchange_rate_kind)

ratio_deflators_table <-
  robustness_table %>%
  filter(
    ratios_kind == "article ratios" &
      (
        ratio_exchange_rate_kind == "PPP GDP" |
          ratio_exchange_rate_kind == "N/A"
      ) &
      (GDP_deflator_kind == "CPI medical") &
      (GDP_exchange_rate_kind == "PPP GDP")
  ) %>%
  select(
    -ratios_kind,
    -ratio_exchange_rate_kind,
    -GDP_deflator_kind,
    -GDP_exchange_rate_kind
  ) %>%
  rename(`Ratio deflators` = ratio_deflator_kind)

ratio_exchange_table <-
  robustness_table %>%
  filter(
    ratios_kind == "article ratios" &
      (ratio_deflator_kind == "CPI medical" | ratio_deflator_kind == "N/A") &
      (GDP_deflator_kind == "CPI medical") &
      (GDP_exchange_rate_kind == "PPP GDP")
  ) %>%
  select(
    -ratios_kind,
    -ratio_deflator_kind,
    -GDP_deflator_kind,
    -GDP_exchange_rate_kind
  ) %>%
  rename(`Ratio exchange rates` = ratio_exchange_rate_kind)
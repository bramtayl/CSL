library(dplyr, warn.conflicts = FALSE)
library(ivreg)
library(readr)

output_folder <- "~/Desktop"

world_bank_data <-
  read_csv(paste0(output_folder, "/filtered_world_bank_data.csv"))

by_country_and_year_filtered <-
  read_csv(paste0(output_folder, "/by_country_and_year.csv")) %>%
  filter(
    strategy == "Base" &
      GDP_exchange_rate_kind == "PPP GDP" &
      GDP_deflator_kind == "GDP deflator" &
      ratio_exchange_rate_kind == "PPP GDP" &
      ratio_deflator_kind == "GDP deflator"
  )

run_with_variable <- function(countries, values, years) {
  present_data <-
    by_country_and_year_filtered %>%
    left_join(
      tibble(
        country = countries,
        value = values,
        year = years
      ),
      by = c("country", "year")
    ) %>%
    filter(!is.na(value))

  if (nrow(present_data) == 0) {
    return(0)
  }

  by_country <-
    present_data %>%
    group_by(country) %>%
    summarize(
      log_price_of_life = weighted.mean(
        log_price_of_life,
        number_of_observations
      ),
      year = weighted.mean(year, number_of_observations),
      life_expectancy = weighted.mean(life_expectancy, number_of_observations),
      log_GDP_per_capita = weighted.mean(
        log_GDP_per_capita,
        number_of_observations
      ),
      value = weighted.mean(
        value,
        number_of_observations
      ),
      number_of_observations = sum(number_of_observations),
      .groups = "drop"
    )

  formula <-
    log_price_of_life ~
    # exogenous
    year +
    value +
    # endogenous
    life_expectancy |
    # exogenous
    year +
    value +
    # instruments
    log_GDP_per_capita

  r_squared <- tryCatch({
    model <-
      ivreg(
        formula,
        data = by_country,
        weights = number_of_observations
      )
    summary(model)$adj.r.squared
  }, warning = function(w) {
    0
  }, error = function(e) {
    0
  })
  tibble(
    present_percentage =
      nrow(present_data) / nrow(by_country_and_year_filtered),
    country_present_percentage =
      nrow(
        present_data %>%
          select(country) %>%
          distinct()
      ) / nrow(
        by_country_and_year_filtered %>%
          select(country) %>%
          distinct()
      ),
    r_squared = r_squared
  )
}

all_results <-
  world_bank_data %>%
  group_by(indicator_name) %>%
  reframe(run_with_variable(country, value, year))

all_results %>%
  filter(
    r_squared != 0 &
      present_percentage > 0.7 &
      country_present_percentage > 0.7
  ) %>%
  arrange(desc(r_squared)) %>%
  View()
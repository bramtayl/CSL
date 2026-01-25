library(dplyr, warn.conflicts = FALSE)
library(plm)
library(readr)
library(tidyr)

output_folder <- "~/Desktop"

world_bank_data <-
  read_csv(paste0(output_folder, "/filtered_world_bank_data.csv")) %>%
  mutate(
    indicator_name = make.names(indicator_name)
  )

base_data <-
  read_csv(paste0(output_folder, "/default_data.csv")) %>%
  select(
    year,
    country,
    life_expectancy,
    log_gdp_per_capita,
    log_price_of_life,
    number_of_observations
  ) %>%
  filter(number_of_observations > 0) %>%
  mutate(year_factor = as.factor(year))

get_country_count <- function(data) {
  nrow(data %>% select(country) %>% distinct())
}

make_panel <- function(data) {
  pdata.frame(data, index = c("country", "year"))
}

make_formula <- function(controls, with_value = FALSE) {
  if (with_value) {
    controls = c(controls, "value")
  }
  if (length(controls) == 0) {
    log_price_of_life ~ life_expectancy + year_factor | log_gdp_per_capita + year_factor
  } else {
  controls %>%
    paste(collapse = " + ") %>%
    paste0(
      "log_price_of_life ~ life_expectancy + ",
      .,
      " + year_factor | log_gdp_per_capita + ",
      .,
      " + year_factor"
    ) %>%
    as.formula
  }
}

get_r_squared <- function(new_data, controls, with_value = FALSE) {
  plm(
    make_formula(controls, with_value = with_value),
    data = make_panel(new_data),
    model = "random"
  ) %>%
    summary() %>%
    .[["r.squared"]] %>%
    .[["adjrsq"]]
}

run_with_variable <- function(
  previous_data,
  controls,
  countries,
  values,
  years
) {
  new_data <-
    previous_data %>%
    left_join(
      tibble(
        country = countries,
        value = values,
        year = years
      ),
      by = c("country", "year")
    ) %>%
    filter(complete.cases(.))

  if (nrow(new_data) == 0) {
    return(
      tibble(
        present_percentage = 0.0,
        country_present_percentage = 0.0,
        r_squared = 0.0
      )
    )
  }

  r_squared <- tryCatch({
    get_r_squared(new_data, controls, with_value = TRUE)
  }, warning = function(w) {
    0.0
  }, error = function(e) {
    0.0
  })

  tibble(
    present_percentage = nrow(new_data) / nrow(previous_data),
    country_present_percentage =
      get_country_count(new_data) /
      get_country_count(previous_data),
    r_squared = r_squared
  )
}

add_control_variables <- function(data, controls, scale = FALSE) {
  long_data <-
    inner_join(
      world_bank_data,
      tibble(indicator_name = controls),
      by = "indicator_name"
    ) %>%
    select(year, country, indicator_name, value)
  
  if (scale == TRUE) {
    long_data <- 
      long_data %>%
      group_by(indicator_name) %>%
      mutate(value = scale(value)) %>%
      ungroup()
  }

  data %>%
    left_join(
      long_data %>%
        pivot_wider(
          names_from = "indicator_name",
          values_from = "value"
        ),
      by = c("country", "year")
  )
}

add_control <- function(controls) {
  previous_data <- 
    add_control_variables(base_data, controls, scale = TRUE) %>%
    filter(complete.cases(.))
  
  all_results <-
    world_bank_data %>%
    anti_join(
      tibble(indicator_name = controls),
      by = "indicator_name"
    ) %>%
    group_by(indicator_name) %>%
    reframe(run_with_variable(
      previous_data,
      controls,
      country,
      value,
      year
    )) %>%
    filter(
      r_squared != 0 &
        present_percentage > 0.8 &
        country_present_percentage > 0.8
    ) %>%
    arrange(desc(r_squared))

  if (nrow(all_results) > 0) {
    print(
      all_results[["r_squared"]][[1]]
    )

    c(controls, all_results[["indicator_name"]][[1]])
  } else {
    c(controls, "ERROR")
  }
}

remove_control <- function(controls) {
  previous_data <- add_control_variables(base_data, controls)
  left_out_data <-
    sapply(
      controls,
      function(left_out) {
        tryCatch({
          get_r_squared(previous_data, setdiff(controls, left_out), with_value = FALSE)
        }, error = function(e) {
          0
        })
      }
    ) %>%
      sort(decreasing = TRUE)
  
  print(left_out_data[[1]])
  take_out = names(left_out_data)[[1]]
  print(take_out)
  setdiff(controls, take_out)
}

controls <- character(0)

# repeat as needed to search for useful controls
controls <- add_control(controls)

controls <- remove_control(controls)

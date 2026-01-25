library(car)
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

all_controls <- make.names(c(
  "Access to electricity (% of population)",
  "Adjusted net enrollment rate, primary (% of primary school age children)",
  "Agricultural raw materials exports (% of merchandise exports)",
  "Average precipitation in depth (mm per year)",
  "Domestic general government health expenditure (% of current health expenditure)",
  "Employment to population ratio, 15+, female (%) (national estimate)",
  "External health expenditure (% of current health expenditure)",
  "Gini index",
  "Industry (including construction), value added (% of GDP)",
  "Low-birthweight babies (% of births)",
  "Mortality caused by road traffic injury (per 100,000 population)",
  "Nurses and midwives (per 1,000 people)",
  "Out-of-pocket expenditure (% of current health expenditure)",
  "People practicing open defecation (% of population)",
  "People using at least basic drinking water services (% of population)",
  "People using at least basic sanitation services (% of population)",
  "People using safely managed drinking water services (% of population)",
  "People using safely managed sanitation services (% of population)",
  "Population ages 0-14 (% of total population)",
  "Population density (people per sq. km of land area)",
  "Physicians (per 1,000 people)",
  "Prevalence of overweight, weight for height (modeled estimate, % of children under 5)",
  "Prevalence of undernourishment (% of population)",
  "Primary education, pupils (% female)",
  "Risk of catastrophic expenditure for surgical care (% of people at risk)",
  "School enrollment, primary and secondary (gross), gender parity index (GPI)",
  "School enrollment, tertiary (% gross)",
  "Share of youth not in education, employment or training, total (% of youth population)  (modeled ILO estimate)",
  "Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)",
  "Unemployment, total (% of total labor force) (modeled ILO estimate)"
))

if (nrow(
  tibble(indicator_name = all_controls) %>%
    anti_join(
      world_bank_data,
      by = "indicator_name"
    )
) > 0) {
  stop("Missing controls")
}

# write a file with indicator codes for these variables
variable_table <-
  base_data %>%
  select(country, year) %>%
  cross_join(
    tibble(indicator_name = all_controls)
  ) %>%
  left_join(
    world_bank_data %>%
      select(indicator_name, country, year, value),
    by = c("indicator_name", "country", "year")
  ) %>%
  left_join(
    world_bank_data %>%
      select(indicator_name, indicator_code) %>%
      distinct
  ) %>%
  group_by(indicator_name) %>%
  summarize(
    percent_present = sum(!is.na(value)) / n(),
    indicator_code = first(indicator_code),
    .groups = "drop"
  )

variable_table %>%
  write_csv(paste0(output_folder, "/variable_table.csv"))

minimally_present_variables <-
  variable_table %>%
  filter(percent_present > 0.7) %>%
  .$indicator_name

data_with_controls <- 
  add_control_variables(base_data, minimally_present_variables)

remove_max_vif <- function(controls, cutoff) {
  while (length(controls) > 0) {
    model <- plm(
      make_formula(controls),
      data = make_panel(data_with_controls),
      model = "random"
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
    
    if (vifs_table$value[1] > cutoff) {
      controls <- setdiff(controls,
        vifs_table %>%
          filter(is_control) %>%
          .$coefficient %>%
          .[1]
      )
    } else {
      break
    }
  }
  controls
}

distinctive_controls <- remove_max_vif(minimally_present_variables, cutoff = 10)

remove_least_significant <- function(controls, p_value_cutoff) {
  while (length(controls) > 0) {
    model <- plm(
      make_formula(controls),
      data = make_panel(data_with_controls),
      model = "random"
    )

    coefficients_table <-
      summary(model)$coefficients %>%
      as_tibble(rownames = "coefficient") %>%
      semi_join(tibble(coefficient = controls), by = "coefficient") %>%
      rename(p_value = `Pr(>|z|)`) %>%
      arrange(desc(p_value))

    if (coefficients_table$p_value[1] > p_value_cutoff) {
      controls <- setdiff(controls, coefficients_table$coefficient[1])
    } else {
      break
    }
  }
  controls
}

add_most_useful <- function(
  controls,
  included_controls
) {
  left_over = setdiff(controls, included_controls)
  r_squareds <- sapply(left_over, function(control) {
    tryCatch({
      plm(
        make_formula(c(included_controls, control)),
        data = panel,
        model = "random"
      ) %>%
      summary() %>%
      .[["r.squared"]] %>%
    .[["adjrsq"]]
    }, warning = function(w) {
      0.0
    }, error = function(e) {
      0.0
    })
  }) %>%
    sort(decreasing = TRUE)

  print(r_squareds[1])
  c(included_controls, names(r_squareds)[1])
}


remove_least_useful <- function(
  controls,
  included_controls
) {
  r_squareds <- sapply(included_controls, function(control) {
    tryCatch({
      plm(
        make_formula(setdiff(included_controls, control)),
        data = panel,
        model = "random"
      ) %>%
      summary() %>%
      .[["r.squared"]] %>%
    .[["adjrsq"]]
    }, warning = function(w) {
      0.0
    }, error = function(e) {
      0.0
    })
  }) %>%
    sort(decreasing = TRUE)

  print(r_squareds[1])
  setdiff(included_controls, names(r_squareds)[1])
}

significant_controls <-
  remove_least_significant(
    distinctive_controls,
    p_value_cutoff = 0.10
  )

random_model <- plm(
  make_formula(significant_controls),
  data = make_panel(data_with_controls),
  model = "random"
)

summary(random_model)
confint(random_model)

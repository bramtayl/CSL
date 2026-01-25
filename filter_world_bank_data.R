library(dplyr, warn.conflicts = FALSE)
library(readr)

output_folder <- "~/Desktop"

# ONLY RUN THIS SECTION ONCE
read_csv(paste0(output_folder, "/all_world_bank_data.csv")) %>%
  select(
    country_code_3 = Country.Code,
    indicator_name = Indicator.Name,
    indicator_code = Indicator.Code,
    year,
    value
  ) %>%
  left_join(
    read_csv(paste0(output_folder, "/country_data.csv")) %>%
      select(country, country_code_3),
    by = "country_code_3"
  ) %>%
  select(-country_code_3) %>%
  semi_join(
    read_csv(paste0(output_folder, "/default_data.csv")) %>%
      filter(number_of_observations > 0) %>%
      select(year, country) %>%
      distinct(),
    by = c("country", "year")
  ) %>%
  write_csv(paste0(output_folder, "/filtered_world_bank_data.csv"))

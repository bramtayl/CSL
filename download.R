library(countrycode)
library(fredr)
library(purrr)
library(readr)
library(rnaturalearth)
library(sf)
library(stringi)
library(tibble)
library(tidyr)
library(wbstats)
library(WDI)

library(dplyr)

output_folder = "~/Desktop"

bulk_data = WDIbulk()

write_csv(bulk_data$Country, "data/world_bank_countries_2.csv")

bulk_data %>%
  filter(!is.na(value)) %>%
  write_csv(paste0(output_folder, "/all_world_bank_data.csv"))

fredr_set_key("859a3bf50a026ca281aaeaf9dd06e3de")

fredr("CPIAUCSL") %>%
  write_csv("data/CPI.csv", na = "")

fredr("GDPDEF")
write_csv(GDP_deflator_data, "data/GDP_deflator.csv", na = "")

fredr("CPIMEDSL") %>%
  write_csv("data/CPI_medical.csv", na = "")

wb_cache()$countries %>%
  write_csv("data/world_bank_countries.csv", na = "")

read_csv("data/selected_world_bank_variables.csv") %>%
  .$variable_code %>%
  map(
    function(variable_code) {
      print(variable_code)
      wb_data(variable_code, country = "all", return_wide = FALSE)
    }
  ) %>%
  list_rbind %>%
  dplyr::filter(!is.na(value) & !is.na(iso2c)) %>%
  write_csv("data/world_bank_data.csv", na = "")

write_csv(codelist, "data/countrycode_data.csv", na = "")

ne_countries(scale = "small", returnclass = "sf") %>%
  st_write(map_data, "data/map_data.gpkg")

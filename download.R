# Downloads the public data in data/. See README.md for the data that must be
# downloaded by hand.
# Needs a free FRED API key (https://fred.stlouisfed.org/docs/api/api_key.html)
# in the FRED_API_KEY environment variable.

library(countrycode)
library(fredr)
library(purrr)
library(readr)
library(rnaturalearth)
library(sf)
library(wbstats)

# load last for filter
library(dplyr, warn.conflicts = FALSE)

fredr_set_key(Sys.getenv("FRED_API_KEY"))

download_fred <- function(series_id, file) {
  fredr(series_id) |>
    write_csv(file.path("data", file), na = "")
}

download_fred("CPIAUCSL", "CPI.csv")
download_fred("GDPDEF", "GDP_deflator.csv")
download_fred("CPIMEDSL", "CPI_medical.csv")

# too much data to download at once, so download variables separately
# might have to try a couple of times
read_csv("data/selected_world_bank_variables.csv", show_col_types = FALSE) |>
  pull(variable_code) |>
  map(
    function(variable_code) {
      message(variable_code)
      wb_data(variable_code, country = "all", return_wide = FALSE)
    }
  ) |>
  list_rbind() |>
  filter(!is.na(value) & !is.na(iso2c)) |>
  write_csv("data/world_bank_data.csv", na = "")

write_csv(codelist, "data/countrycode_data.csv", na = "")

ne_countries(scale = "small", returnclass = "sf") |>
  st_write("data/map_data.gpkg", delete_dsn = TRUE)

library(countrycode)
library(dplyr)
library(FAOSTAT)
library(fredr)
library(readr)
library(rnaturalearth)
library(sf)
library(stringi)
library(tibble)
library(tidyr)
library(WDI)
    
FRED_START_DATE = as.Date("1947-01-01")
FRED_END_DATE = as.Date("2022-12-31")

CPI_data = 
    fredr(
        series_id = "CPIAUCSL",
        observation_start = FRED_START_DATE,
        observation_end = FRED_END_DATE
    )

write_csv(CPI_data, "data/CPI.csv", na = "")

GDP_deflator_data = 
    fredr(
        series_id = "GDPDEF",
        observation_start = FRED_START_DATE,
        observation_end = FRED_END_DATE
    )

write_csv(GDP_deflator_data, "data/GDP_deflator.csv", na = "")

all_WDI_variables = as_tibble(WDI_data$series)
write_csv(all_WDI_variables, "data/all_WDI_variables.csv", na = "")
selected_WDI_variables = read_csv("data/selected_WDI_variables.csv", na = "")

WDI_variable_details =
    selected_WDI_variables %>%
    left_join(all_WDI_variables)

if (any(is.na(WDI_variable_details$indicator))) {
    stop("Missing indicators!")
}

WDI_downloads = WDI(WDI_variable_details$indicator, country = "all")
write_csv(WDI_downloads, "data/WDI_data.csv", na = "")

write_csv(codelist, "data/countries.csv", na = "")

map_data = 
    ne_countries(scale = "small", returnclass = "sf") %>%
    # transform to a more common CRS so GDAL doesn't get confused
    st_transform(crs = "EPSG:4326")

st_write(map_data, "data/map_data.gpkg")

# optional
# download and extract
# http://databank.worldbank.org/data/download/WDI_csv.zip

WDI_path = "C:\\Users\\brand\\WDI_csv"

download.file(
    "https://climatedata.worldbank.org/thredds/fileServer/CRM/cru/cru-ts4.06-timeseries/tas/mean/annual/timeseries-tas-annual-mean/1901-2021/timeseries-tas-annual-mean_cru_annual_cru-ts4.06-timeseries_mean_1901-2021.nc",
    "data/average_temperature_by_year.nc"
)
download.file(
    "https://climatedata.worldbank.org/thredds/fileServer/CRM/cru/cru-ts4.06-timeseries/pr/mean/annual/timeseries-pr-annual-mean/1901-2021/timeseries-pr-annual-mean_cru_annual_cru-ts4.06-timeseries_mean_1901-2021.nc",
    "data/yearly_precipitation.nc"
)

read_csv(file.path(WDI_path, "WDIData.csv"), guess_max = Inf) %>%
    select(`Indicator Name`, `1960`:`2021`) %>%
    pivot_longer(
        `1960`:`2021`,
        names_to = "Year",
        values_to = "Value"
    ) %>%
    group_by(`Indicator Name`) %>%
    summarize(number_of_values = sum(!is.na(Value))) %>%
    arrange(desc(number_of_values)) %>%
    write_csv(file.path(WDI_path, "variable_info.csv"))

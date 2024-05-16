library(countrycode)
library(dplyr)
library(fredr)
library(readr)
library(rnaturalearth)
library(sf)
library(stringi)
library(tibble)
library(tidyr)
library(wbstats)

fredr_set_key("859a3bf50a026ca281aaeaf9dd06e3de")

CPI_data = fredr("CPIAUCSL")
write_csv(CPI_data, "data/CPI.csv", na = "")

GDP_deflator_data = fredr("GDPDEF")
write_csv(GDP_deflator_data, "data/GDP_deflator.csv", na = "")

CPI_medical_data = fredr("CPIMEDSL")
write_csv(CPI_medical_data, "data/CPI_medical.csv", na = "")

all_WB_variables <- wb_cache()$indicators
write_csv(all_WDI_variables, "data/all_WB_variables.csv", na = "")

indicator_ids = c(
    "AG.LND.PRCP.MM", # annual_precipitation
    "SH.XPD.GHED.CH.ZS", # percent_health_spending_public
    "NY.GDP.PCAP.CN", # local_historical_GDP_per_capita
    "SP.DYN.LE00.IN", # life_expectancy 
    "PA.NUS.FCRF", # nominal_exchange_rate
    "EN.POP.DNST", # population_density
    "PA.NUS.PPP", # PPP_exchange_rate_GDP
    "PA.NUS.PRVT.PP" # PPP_exchange_rate_private_consumption,
    # wb_data is not working with series 9080000
    # had to download health.csv manually
    # "9080000" # PPP_exchange_rate_health
)

WB_downloads = wb_data(indicator_ids, country = "all")
write_csv(WB_downloads, "data/WB_data.csv", na = "")

write_csv(codelist, "data/countrycode_data.csv", na = "")

map_data = ne_countries(scale = "small", returnclass = "sf")

st_write(map_data, "data/map_data.gpkg")

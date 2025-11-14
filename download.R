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
library(WDI)

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

wb_data(c(
    "AG.LND.PRCP.MM", # annual_precipitation
    "SH.XPD.GHED.CH.ZS", # percent_health_spending_public
    "NY.GDP.PCAP.CN", # local_historical_GDP_per_capita
    "SP.DYN.LE00.IN", # life_expectancy
    "SE.TER.ENRR.FE", # female_education
    "SM.POP.NETM", # net migration
    "PA.NUS.FCRF", # nominal_exchange_rate
    "SP.POP.TOTL", # population
    "EN.POP.DNST", # population_density
    "PA.NUS.PPP", # PPP_exchange_rate_GDP
    "PA.NUS.PRVT.PP", # PPP_exchange_rate_private_consumption,
    "SE.XPD.TOTL.GB.ZS" # percent_spending_education
    # wb_data is not working with series 9080000
    # had to download health.csv manually
  ),
  country = "all") %>%
  write_csv("data/world_bank_data.csv", na = "")

write_csv(codelist, "data/countrycode_data.csv", na = "")

ne_countries(scale = "small", returnclass = "sf") %>%
  st_write(map_data, "data/map_data.gpkg")

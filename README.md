# The marginal cost of life

Code and data for "The marginal cost of life" by Brandon Taylor.

## Data

Most of the data are in `data/`:

| File | Source | How to get it |
| --- | --- | --- |
| `CPI.csv`, `CPI_medical.csv`, `GDP_deflator.csv` | US Federal Reserve (FRED) series `CPIAUCSL`, `CPIMEDSL`, `GDPDEF` | `download.R` |
| `world_bank_data.csv` | World Bank World Development Indicators, for the variables in `selected_world_bank_variables.csv` | `download.R` |
| `countrycode_data.csv` | the `countrycode` R package | `download.R` |
| `map_data.gpkg` | Natural Earth, via the `rnaturalearth` R package | `download.R` |
| `health_2005.csv`, `health_2011.csv`, `health_2017.csv`, `health_2021.csv` | World Bank International Comparison Program, series `9080000:ACTUAL HEALTH`, for each ICP base year | downloaded by hand from DataBank |
| `health_adjusted_life_expectancy.csv` | World Health Organization Global Health Observatory, indicator `WHOSIS_000002` | downloaded by hand |
| `country_replacements.csv`, `currency_country_replacements.csv` | written by hand, to match country names in the CEA Registry to other sources | |
| `control_reasons.csv` | written by hand: why each control might shift the marginal cost of life curve | |
| `ochalek_2018_cost_per_daly.csv` | Ochalek, Lomas and Claxton (2018), BMJ Global Health 3:e000964, online supplementary file 1: cost per DALY averted by country, 2015 US$, for four methods (CC BY) | copied by hand from the supplementary file |

The price data come from the Tufts Medical Cost-Effectiveness Analysis (CEA) Registry.
They are licensed, so they aren't in this repository.
To reproduce the results, obtain access to the registry and save its exports as `data/ratios.csv` and `data/methods.csv`.

## Running

Package versions are recorded with [renv](https://rstudio.github.io/renv/).
Run `renv::restore()` in the project folder to install them.
The results were produced with R 4.5.2.

1. Optionally, run `download.R` to download the public data again. This needs a free [FRED API key](https://fred.stlouisfed.org/docs/api/api_key.html) in the `FRED_API_KEY` environment variable.
2. Run `bayesian_code.R` to fit the hierarchical Bayesian model in `random_effects.stan`. This needs [CmdStan](https://mc-stan.org/cmdstanr/), and takes a while. It saves the fit to `output/stan_fit.RDS`.
3. Knit `CSL.Rtex` with `knitr::knit("CSL.Rtex")`, then compile `CSL.tex` with `latexmk -pdf CSL.tex`. Knitting sources `processing.R`, which cleans the data and selects controls, and `analysis.R`, which fits the models and makes the tables and figures.
4. Optionally, run `pandoc.sh` to convert the paper to Word.

Intermediate data and diagnostics, such as countries that couldn't be matched across sources, are written to `output/`.

## Other files

`exploration/` contains earlier scripts I used to search for control variables.
They aren't needed to reproduce the results, and they depend on an older version of the data pipeline.

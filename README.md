
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rpicsa: R Package for PICSA and Climatic Analysis for Crops

<!-- badges: start -->

[![R-CMD-check](https://github.com/IDEMSInternational/rpicsa/workflows/R-CMD-check/badge.svg)](https://github.com/IDEMSInternational/rpicsa/actions)
[![Codecov test
coverage](https://codecov.io/gh/IDEMSInternational/rpicsa/branch/main/graph/badge.svg)](https://app.codecov.io/gh/IDEMSInternational/rpicsa?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![license](https://img.shields.io/badge/license-LGPL%20(%3E=%203)-lightgrey.svg)](https://www.gnu.org/licenses/lgpl-3.0.en.html)
<!-- badges: end -->

## Overview

**rpicsa** is an R package designed to assist in the analysis of climate
data for crop production using the Plant Impact on Climate and Societal
Adaptation (PICSA) framework. It provides a range of functions to help
researchers, agronomists, and farmers make informed decisions about
planting and managing crops based on climatic data.

The package offers a suite of tools to perform various climatic
analyses, including:

- Start of Rains: Determine the onset of the rainy season, an important
  factor in crop planning.
- End of Rains: Identify when the rains conclude, helping in crop
  harvesting and preparation for the dry season.
- End of Season: Identify when the rainy season concludes, helping in
  crop harvesting and preparation for the dry season.
- Length of Season: Calculate the duration of the growing season, a
  useful parameter for crop selection.
- Rainfall Summaries: Generate summaries of rainfall within a rainy
  season to understand precipitation variability.
- Temperature Summaries: Calculate temperature statistics to assess the
  impact on crop development.
- Crop Success Probability: Estimate the likelihood of successful crop
  production based on historical climate data.
- Season Start Probability: Predict the probability of the rainy
  season’s onset to optimise planting schedules.

## Installation

You can install the development version of `rpicsa` like so:

``` r
# Install the 'devtools' package if you haven't already
if (!require(devtools)) {
  install.packages("devtools")
}

# Install 'rpicsa' from GitHub
devtools::install_github("IDEMSInternational/rpicsa")
```

## Usage

We give some small examples in using the functions in `rpicsa`

``` r
library(rpicsa)
library(dplyr)

# use the data in the package, but for this demonstration we will look at the data before 1950.
daily_niger <- rpicsa::daily_niger %>% dplyr::filter(year <= 1950)
```

To calculate the start of rains in Niger, we can use the `start_rains()`
function. In our case, we define the start of rains as the first
instance where there is more than 25mm of rainfall over three days, and
there is no more than nine days without rainfall within 21 days.

``` r
start_rains(data = daily_niger, date_time = "date", station = "station_name",
            year = "year", doy = "doy", rain = "rain",
            total_rainfall = TRUE, over_days = 3, amount_rain = 25,
            dry_spell = TRUE, spell_interval = 21, spell_max_dry_days = 9
            )
```

| station_name  | year | start_rains |
|:--------------|-----:|------------:|
| Agades        | 1945 |          NA |
| Agades        | 1946 |          NA |
| Agades        | 1947 |         212 |
| Agades        | 1950 |         231 |
| Birni N’Konni | 1945 |          NA |
| Birni N’Konni | 1946 |         156 |
| Birni N’Konni | 1947 |         180 |
| Birni N’Konni | 1948 |         195 |
| Birni N’Konni | 1949 |         188 |
| Birni N’Konni | 1950 |         157 |
| Niamey_Aero   | 1940 |          NA |
| Niamey_Aero   | 1941 |          NA |
| Niamey_Aero   | 1942 |          NA |
| Niamey_Aero   | 1943 |         144 |
| Niamey_Aero   | 1944 |          NA |
| Niamey_Aero   | 1945 |         170 |
| Niamey_Aero   | 1946 |         166 |
| Niamey_Aero   | 1947 |         166 |
| Niamey_Aero   | 1948 |         160 |
| Niamey_Aero   | 1949 |         206 |
| Niamey_Aero   | 1950 |         188 |
| Zinder        | 1945 |          NA |
| Zinder        | 1946 |         149 |
| Zinder        | 1947 |         183 |
| Zinder        | 1948 |         183 |
| Zinder        | 1949 |         203 |
| Zinder        | 1950 |         201 |

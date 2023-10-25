
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

We also calculate the end of rains. Here, we define the end of rains to
be the first instance after day 121 (April 30th), where there is one day
with no more than 10mm of rainfall.

``` r
# Calculate Start of Rains
start_rain_table <- start_rains(data = daily_niger, date_time = "date", station = "station_name",
            year = "year", doy = "doy", rain = "rain",
            total_rainfall = TRUE, over_days = 3, amount_rain = 25,
            dry_spell = TRUE, spell_interval = 21, spell_max_dry_days = 9
            )

# Calculate End of Rains
end_rain_table <- end_rains(data = daily_niger, date_time = "date", station = "station_name",
            year = "year", doy = "doy", rain = "rain",
            start_day = 121, interval_length = 1, min_rainfall = 10)

# Combine into a table Start of Rains
summary_rains_table <- dplyr::full_join(start_rain_table, end_rain_table)
```

We can then use this to calculate other summary statistics - for
example, length of season, or rain summaries in the season

``` r
# Length of season
season_length_table <- rpicsa::seasonal_length(summary_rains_table, start_date = "start_rains", end_date = "end_rains",
                                               data = daily_niger, date_time = "date", station = "station_name",
            year = "year", doy = "doy", rain = "rain")

# We can also look at the total rainy days and rainfall amount in each season
season_rain_table <- rpicsa::seasonal_rain(summary_rains_table, start_date = "start_rains", end_date = "end_rains",
                                               data = daily_niger, date_time = "date", station = "station_name",
            year = "year", doy = "doy", rain = "rain")
```

| Station       | Year | Start of rains | End of rains | Length of season | Rain in season (mm) | Number of rainy days in season |
|:--------------|-----:|---------------:|-------------:|-----------------:|--------------------:|-------------------------------:|
| Agades        | 1945 |             NA |           NA |               NA |                  NA |                             NA |
| Agades        | 1946 |             NA |          239 |               NA |                  NA |                             NA |
| Agades        | 1947 |            212 |          292 |               80 |               264.9 |                             22 |
| Agades        | 1950 |            231 |          255 |               24 |               154.2 |                              7 |
| Birni N’Konni | 1945 |             NA |          274 |               NA |                  NA |                             NA |
| Birni N’Konni | 1946 |            156 |          292 |              136 |                  NA |                             NA |
| Birni N’Konni | 1947 |            180 |          265 |               85 |               478.9 |                             31 |
| Birni N’Konni | 1948 |            195 |          255 |               60 |               318.2 |                             16 |
| Birni N’Konni | 1949 |            188 |          261 |               73 |               325.9 |                             26 |
| Birni N’Konni | 1950 |            157 |          262 |              105 |               702.8 |                             41 |
| Niamey_Aero   | 1940 |             NA |           NA |               NA |                  NA |                             NA |
| Niamey_Aero   | 1941 |             NA |           NA |               NA |                  NA |                             NA |
| Niamey_Aero   | 1942 |             NA |           NA |               NA |                  NA |                             NA |
| Niamey_Aero   | 1943 |            144 |          263 |              119 |                  NA |                             NA |
| Niamey_Aero   | 1944 |             NA |           NA |               NA |                  NA |                             NA |
| Niamey_Aero   | 1945 |            170 |          261 |               91 |               562.9 |                             39 |
| Niamey_Aero   | 1946 |            166 |          299 |              133 |               707.3 |                             46 |
| Niamey_Aero   | 1947 |            166 |          312 |              146 |               396.4 |                             33 |
| Niamey_Aero   | 1948 |            160 |          255 |               95 |               541.4 |                             28 |
| Niamey_Aero   | 1949 |            206 |          253 |               47 |               268.8 |                             25 |
| Niamey_Aero   | 1950 |            188 |          280 |               92 |               573.5 |                             41 |
| Zinder        | 1945 |             NA |          261 |               NA |                  NA |                             NA |
| Zinder        | 1946 |            149 |          272 |              123 |               761.4 |                             42 |
| Zinder        | 1947 |            183 |          269 |               86 |               414.0 |                             30 |
| Zinder        | 1948 |            183 |          264 |               81 |               304.6 |                             24 |
| Zinder        | 1949 |            203 |          255 |               52 |               207.4 |                             14 |
| Zinder        | 1950 |            201 |          269 |               68 |               541.3 |                             24 |
| Agades        | 1948 |             NA |          227 |               NA |                  NA |                             NA |
| Agades        | 1949 |             NA |          225 |               NA |                  NA |                             NA |

We can use this to calculate probabilities to aid in successful crop
production in the future. Let’s say we are just interested in
`Birni N'Konni`. Say we want to find the best crop to plant, so try out
different planting days and planting periods to see which would
historically be most likely to succeed. We look at 100mm rainfall for a
crop where the start of rains starts before day 100, and then before day
200, and where the length of season is 60 days or 80 days:

``` r
crops_definitions(data = daily_niger %>% dplyr::filter(station_name == "Birni N'Konni"), date_time = "date", station = "station_name", year = "year", doy = "doy", rain = "rain",
                  season_data = summary_rains_table %>% dplyr::filter(station_name == "Birni N'Konni"), start_day = "start_rains", end_day = "end_rains",
                  water_requirements = 100,
                  planting_dates = c(100, 200),
                  planting_length = c(60, 80))
```

| Station       | Water Requirements | Planting Day | Season Length | Probability of Success |
|:--------------|-------------------:|-------------:|--------------:|-----------------------:|
| Birni N’Konni |                100 |          100 |            60 |                   0.00 |
| Birni N’Konni |                100 |          100 |            80 |                   0.00 |
| Birni N’Konni |                100 |          200 |            60 |                   0.80 |
| Birni N’Konni |                100 |          200 |            80 |                   0.17 |

We can see that the most successful crop will be one that needs a
planting length of 60 days, and can be planted after day 200.

We can finally look at the probability of the rainy season to start
after a specified set of dates:

``` r
probability_season_start(summary_rains_table %>% dplyr::filter(station_name == "Birni N'Konni"), station = "station_name", start_rains = "start_rains", specified_day = c(100, 200, 300))
```

| Station       | Start day | Probability |
|:--------------|:----------|------------:|
| Birni N’Konni | 100       |        0.00 |
| Birni N’Konni | 200       |        0.83 |
| Birni N’Konni | 300       |        0.83 |

In Birni N’Konni (from 1945-1950), the rainy season started between day
100 and 200 about 83.3% of the years. This can be useful to aid in
planting schedules.

## Documentation

For detailed information on each function, including arguments, usage,
and examples, please refer to the package documentation. You can access
it using the following command:

``` r
?rpicsa
```

## Contributing

We welcome contributions from the community. If you have any bug
reports, feature requests, or would like to contribute code to rpicsa,
please visit our GitHub repository.

## License

This package is open-source and distributed under the GNU Lesser General
Public License v3.0 License. Feel free to use, modify, and distribute it
as needed.

## Contact

If you have any questions, suggestions, or feedback, please feel free to
reach out to us.

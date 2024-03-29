% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{daily_niger}
\alias{daily_niger}
\title{Daily weather measurements from Niger}
\format{
A data frame with 54423 rows and 14 variables:
\describe{
\item{station_name}{the name of the weather station}
\item{date}{date of the measurements}
\item{year}{year, numeric}
\item{month}{month, factor}
\item{doy}{day of the year, a number from 1 to 366}
\item{day}{day of the month, a number from 1 to 31}
\item{tmax}{daily maximum temperature, in degrees Celsius}
\item{tmin}{daily maximum temperature, in degrees Celsius}
\item{rain}{daily total rainfall, in mm}
\item{hmax}{daily maximum relative humidity, percentage}
\item{hmin}{daily minimum relative humidity, percentage}
\item{sunh}{daily number of sunshine hours}
\item{ws}{daily mean wind speed, m/s}
\item{wd}{daily mean wind direction, degrees}
}
}
\source{
Meteo-France \url{https://meteofrance.com/}
}
\usage{
daily_niger
}
\description{
A dataset containing daily measurements of eight climatic elements from four
weather stations in Niger from 1940 to 1980.
}
\keyword{datasets}

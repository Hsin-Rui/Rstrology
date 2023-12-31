% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{cities}
\alias{cities}
\title{Cities data}
\format{
\subsection{\code{cities}}{

A data frame with 145,536 rows and 2 columns:
\describe{
\item{country}{Country name}
\item{city}{name of city, lat: latitude, lng: longitude.}
\item{tz}{timezone}
\item{population}{population of the city}
\item{big_city}{boolean, population > 300000}

}
}
}
\source{
\url{https://www.geonames.org/}

\url{https://api.opencube.tw/twzipcode}
}
\usage{
cities
}
\description{
Cities having population > 1000 along with country name. Latitude and longitude are included in names for astrological calculation.
}
\keyword{datasets}

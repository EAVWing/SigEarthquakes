% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MakeMap.R
\name{eq_create_label}
\alias{eq_create_label}
\title{Creates a popup label for the map}
\usage{
eq_create_label(data)
}
\arguments{
\item{data}{A data frame containing cleaned NOAA earthquake data}
}
\value{
A character vector for the popup
}
\description{
This puts the location name, magnitude and casualties into a popup
}
\details{
The input data should include the columns LOCATION_NAME,
EQ_PRIMARY and TOTAL_DEATHS.
}
\examples{
\dontrun{
eq_create_label(data)
}

}

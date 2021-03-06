% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetAndClean.R
\name{getData}
\alias{getData}
\title{getData function downloads the data and loads the data frame into memory}
\usage{
getData()
}
\value{
raw data data frame
}
\description{
This function will download the Significant Earthquake Database from the National Oceanic and Atmospheric Administration's (NOAA) website if it doesn't already exist in the working directory and load it as a data frame in the global environment.
}
\examples{
\dontrun{
rawData <- getData()
}

}

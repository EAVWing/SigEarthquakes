% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MakeMap.R
\name{eq_map}
\alias{eq_map}
\title{Map of earthquakes}
\usage{
eq_map(data, annot_col)
}
\arguments{
\item{data}{the cleaned earthquake data from eq_clean_date(getData())}

\item{annot_col}{A character. The name of the column in the data that should
be used as descriptor.}
}
\value{
A leaflet map with earthquakes and annotations.
}
\description{
This function will create a leaflet map of selected earthquakes.
}
\examples{
\dontrun{
eq_clean_data(getData()) \%>\%
dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) \%>\%
dplyr::mutate(popup_text = eq_create_label(.)) \%>\%
eq_map(annot_col = "popup_text")
}

}

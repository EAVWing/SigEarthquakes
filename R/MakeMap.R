#' Map of earthquakes
#'
#' This function will create a leaflet map of selected earthquakes.
#'
#' @param data the cleaned earthquake data from eq_clean_date(getData())
#'
#' @param annot_col A character. The name of the column in the data that should
#' be used as descriptor.
#'
#' @return A leaflet map with earthquakes and annotations.
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#'
#' @examples
#' \dontrun{
#' eq_clean_data(getData()) %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' dplyr::mutate(popup_text = eq_create_label(.)) %>%
#' eq_map(annot_col = "popup_text")
#' }
#'
#' @export
eq_map <- function(data, annot_col) {

    m <- leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addCircleMarkers(lng = data$LONGITUDE, lat = data$LATITUDE,
                                  radius = data$EQ_PRIMARY, weight = 1,
                                  popup = data[[annot_col]])

    m
}


#' Creates a popup label for the map
#'
#' @description  This puts the location name, magnitude and casualties into a popup
#'
#' @param data A data frame containing cleaned NOAA earthquake data
#'
#' @return A character vector for the popup
#'
#' @details The input data should include the columns LOCATION_NAME,
#' EQ_PRIMARY and TOTAL_DEATHS.
#'
#' @examples
#' \dontrun{
#' eq_create_label(data)
#' }
#'
#' @export
eq_create_label <- function(data) {
    popup_text <- with(data, {
        part1 <- ifelse(is.na(LOCATION_NAME), "",
                        paste("<strong>Location:</strong>",
                              LOCATION_NAME))
        part2 <- ifelse(is.na(EQ_PRIMARY), "",
                        paste("<br><strong>Magnitude</strong>",
                              EQ_PRIMARY))
        part3 <- ifelse(is.na(TOTAL_DEATHS), "",
                        paste("<br><strong>Total deaths:</strong>",
                              TOTAL_DEATHS))
        paste0(part1, part2, part3)
    })
}

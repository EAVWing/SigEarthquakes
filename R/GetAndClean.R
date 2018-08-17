#' getData function downloads the data and loads the data frame into memory
#'
#' This function will download the Significant Earthquake Database from the National Oceanic and Atmospheric Administration's (NOAA) website if it doesn't already exist in the working directory and load it as a data frame in the global environment.
#'
#' @return raw data data frame
#'
#' @importFrom readr read_tsv
#'
#' @importFrom utils download.file
#'
#' @examples
#' \dontrun{
#' rawData <- getData()
#'}
#'
#' @export
getData <- function() {

    filename <- "signif.txt"

    if (!file.exists(filename)) {

        fileURL <-"https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt"

        download.file(fileURL, filename)
    }
    data <- suppressMessages(readr::read_tsv(filename))

    data
    #assign("data", readr::read_tsv(filename), envir = globalenv())
}

#' eq_clean_data function takes raw NOAA data data frame and returns a data frame with the Date field added.
#'
#' @return A copy of the data data frame with the Date field added
#'
#' @importFrom lubridate ymd round_date
#'
#' @importFrom dplyr mutate_
#'
#' @param data A data frame with raw data obtained from NOAA website
#'
#' @examples
#' \dontrun{
#' eq_clean_data(getData())
#'}
#'
#' @export
eq_clean_data <- function(data) {

        data$MONTH[is.na(data$MONTH)] <- 1

        data$DAY[is.na(data$DAY)] <- 1

        data$YEAR <- formatC(data$YEAR, width = 4, flag = 0)

        data[, "DATE"] <- NA

        data$DATE <- suppressWarnings(lubridate::ymd(paste(data$YEAR, "/", data$MONTH, "/", data$DAY)))

        y <- lubridate::ymd("0000-01-01") -
                (abs(as.numeric(data$YEAR)) * 365) -
                (abs(as.numeric(data$YEAR)) / 4) +
                .75 * (abs(as.numeric(data$YEAR)) / 100)

        data$DATE[which(as.numeric(data$YEAR) < 0)] =
                y[which(as.numeric(data$YEAR) < 0)]

        z <- lubridate::round_date(data$DATE, unit = "year") +
                julian(as.Date(paste0("1970-", data$MONTH, "-", data$DAY)))

        data$DATE[which(as.numeric(data$YEAR) < 0)] =
                z[which(as.numeric(data$YEAR) < 0)]

        z1 <- z + 1

        data$DATE[which(as.numeric(data$YEAR) < 0 &
                ((as.numeric(data$YEAR) %% 4 == 0 & as.numeric(data$YEAR) %% 100 != 0) |
                as.numeric(data$YEAR) %% 400 == 0) & data$MONTH > 2)] =
                z1[which(as.numeric(data$YEAR) < 0 & ((as.numeric(data$YEAR) %% 4 == 0 &
                as.numeric(data$YEAR) %% 100 != 0) | as.numeric(data$YEAR) %% 400 == 0) &
                data$MONTH > 2)]

        data <- data %>%
            dplyr::mutate_(LATITUDE = ~as.numeric(LATITUDE),
                           LONGITUDE = ~as.numeric(LONGITUDE))

        data <- eq_location_clean(data)

        data
}

#' eq_location_clean function takes raw LOCATION NAME from the data data frame and returns a clean field.
#'
#' @return modified data data frame with the LOCATION_NAME in title case and the country name removed from the LOCATION NAME field
#'
#' @importFrom stringr str_to_title str_replace str_trim
#'
#' @importFrom dplyr mutate_ %>%
#'
#' @param data A data frame with raw data obtained from NOAA website
#'
#' @examples
#' \dontrun{
#' eq_location_clean(data)
#'}
#'
#' @export
eq_location_clean <- function(data) {

    data <- data %>%
        dplyr::mutate_(LOCATION_NAME = ~LOCATION_NAME %>%
                           stringr::str_replace(paste0(COUNTRY, ":"), "") %>%
                           stringr::str_trim("both") %>%
                           stringr::str_to_title())
    data
}

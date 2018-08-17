#' getSignif function downloads the data and loads the data frame into memory
#'
#' This function will download the Significant Earthquake Database from the National Oceanic and Atmospheric Administration's (NOAA) website if it doesn't already exist in the working directory and load it as a data frame in the global environment.
#'
#' @return raw signif data frame
#'
#' @importFrom readr read_tsv
#'
#' @examples{
#' \dontrun{
#' getSignif()
#'}}
#'
#' @export
getSignif <- function() {

    filename <- "signif.txt"

    if (!file.exists(filename)) {

        fileURL <-"https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt"

        download.file(fileURL, filename)
    }

    assign("signif", readr::read_tsv(filename), envir = globalenv())
}

#' eq_clean_data function takes raw NOAA signif data frame and returns a data frame with the Date field added.
#'
#' @return Ac copy of the signif data frame with the Date field added
#'
#' @importFrom lubridate ymd round_date
#'
#' @examples{
#' \dontrun{
#' eq_clean_data()
#'}}
#'
#' @export
eq_clean_data <- function() {

        signif$MONTH[is.na(signif$MONTH)] <- 1

        signif$DAY[is.na(signif$DAY)] <- 1

        signif$YEAR <- formatC(signif$YEAR, width = 4, flag = 0)

        signif[, "date"] <- NA

        signif$date <- suppressWarnings(lubridate::ymd(paste(signif$YEAR, "/", signif$MONTH, "/", signif$DAY)))

        y <- lubridate::ymd("0000-01-01") -
                (abs(as.numeric(signif$YEAR)) * 365) -
                (abs(as.numeric(signif$YEAR)) / 4) +
                .75 * (abs(as.numeric(signif$YEAR)) / 100)

        signif$date[which(as.numeric(signif$YEAR) < 0)] =
                y[which(as.numeric(signif$YEAR) < 0)]

        z <- lubridate::round_date(signif$date, unit = "year") +
                julian(as.Date(paste0("1970-", signif$MONTH, "-", signif$DAY)))

        signif$date[which(as.numeric(signif$YEAR) < 0)] =
                z[which(as.numeric(signif$YEAR) < 0)]

        z1 <- z + 1

        signif$date[which(as.numeric(signif$YEAR) < 0 &
                ((as.numeric(signif$YEAR) %% 4 == 0 & as.numeric(signif$YEAR) %% 100 != 0) |
                as.numeric(signif$YEAR) %% 400 == 0) & signif$MONTH > 2)] =
                z1[which(as.numeric(signif$YEAR) < 0 & ((as.numeric(signif$YEAR) %% 4 == 0 &
                as.numeric(signif$YEAR) %% 100 != 0) | as.numeric(signif$YEAR) %% 400 == 0) &
                signif$MONTH > 2)]

        assign("signif", signif, envir = globalenv())
}

#' eq_location_clean function takes raw LOCATION NAME from the signif data frame and returns a clean field.
#'
#' @return modified signif data frame with the LOCATION_NAME in title case and the country name removed from the LOCATION NAME field
#'
#' @importFrom stringr str_to_title str_replace
#'
#' @examples{
#' \dontrun{
#' eq_location_clean()
#'}}
#'
#' @export
eq_location_clean <- function() {

        i <- 1

        for (i in 1:length(signif$LOCATION_NAME)) {

                signif$LOCATION_NAME[i] <- str_replace(signif$LOCATION_NAME[i],paste0(signif$COUNTRY[i], ": "),"")

                signif$LOCATION_NAME[i] <- str_to_title(signif$LOCATION_NAME[i])

                i <- i + 1
        }
        assign("signif", signif, envir = globalenv())
}

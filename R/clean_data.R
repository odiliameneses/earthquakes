utils::globalVariables(c("date", "dy", "mo", "year", "Location_Name","latitude","longitude","mag", "location_name", "total_deaths"))

#' Clean the LOCATION_NAME column
#'
#' cleans the LOCATION_NAME column by stripping out the country name (including the colon) and converts names to title case (as opposed to all caps).
#'
#' @param rawdata a data frame obtained from NOAA website. Alternatively, this package already contains a dataset from NOAA. Check raw_data for details.
#'
#' @details This function should be applied to the raw data to produce a cleaned up version of the LOCATION_NAME column.
#'
#' @return a data frame with a cleaned up version of the Location_Name column.
#'
#' @import dplyr stringr tidyr
#'
#' @examples
#' \dontrun{
#' raw_data <- readr::read_delim("earthquakes.tsv", "\t") %>%
#' eq_location_clean()
#' }
#'
#'@export
eq_location_clean <- function(rawdata) {
  data <- suppressWarnings({
    rawdata %>%
      tidyr::separate("Location Name",
                      into = c("Location_Name", NA),
                      sep = "[[:punct:];:]") %>%
      dplyr::mutate(Location_Name = stringr::str_to_title(Location_Name))
  })
  dplyr::as_tibble(data)
}

#' Clean data frame
#'
#' Takes raw NOAA data frame and returns a clean data frame.
#'
#' @param rawdata a data frame obtained from NOAA website.
#'
#'
#' @return A date column created by uniting the year, month, day and converting it to the Date class. Plus, LATITUDE and LONGITUDE columns converted to numeric class.
#'
#' @import dplyr janitor
#'
#' @examples
#' \dontrun{
#' raw_data <- readr::read_delim("earthquakes.tsv", "\t")
#' eq_clean_data(raw_data)
#' }
#'
#' @export

eq_clean_data <- function(rawdata) {
  data <- suppressMessages({
    rawdata[-1,-1] %>%
      janitor::clean_names() %>%
      dplyr::mutate(
        date = as.Date(ISOdate(
          year = year,
          month = mo,
          day = dy
        )),
        latitude = as.numeric(latitude),
        longitude = as.numeric(longitude),
        mag = as.numeric(mag),
        total_deaths = as.numeric(total_deaths)
      ) %>%
      dplyr::select(date, everything())
  })
  dplyr::as_tibble(data)
}

#' Data on earthquakes
#'
#' The dataset is based on NOAA website.
#'
#' @format A data frame with 3 755 rows and 9 variables.
#' \describe{
#'   \item{date}{The date of the event as YYYY-MM-DD. (from 1900-01-01 until 2021-04-28)}
#'   \item{country}{The country where the earthquake effects were observed.}
#'   \item{region}{Regional boundaries defined by NOAA. Please consult the NOAA website.}
#'   \item{area}{For US territory:The State, Province or Prefecture where the earthquake effects were observed. Please consult the NOAA website for more information.}
#'   \item{location_name}{Per NOAA website: "The Country, State, Province or City where the Earthquake occurred (For example enter USA or California or San Francisco). This is only an approximate geographic location.(...)"}
#'   \item{latitude}{Latitude: 0 to 90 (Northern Hemisphere) -90 to 0 (Southern Hemisphere).}
#'   \item{longitude}{Longitude: 0 to 180 (Eastern Hemisphere) -180 to 0 (Western Hemisphere).}
#'   \item{mag}{The value in this column contains the primary earthquake magnitude. (0.0 to 9.9)}
#'   \item{total_deaths}{Total Number of Deaths from the Earthquake and secondary effects (eg Tsunami)}
#'}
#' @source National Geophysical Data Center / World Data Service (NGDC/WDS): NCEI/WDS Global Significant Earthquake Database. NOAA National Centers for Environmental Information. doi:10.7289/V5TD9V7K .
#' @usage data(earthquakes)
"earthquakes"

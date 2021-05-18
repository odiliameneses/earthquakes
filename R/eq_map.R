#' Map earthquake epicenters with annotations
#'
#' eq_map() that takes an argument data containing the filtered data frame with earthquakes to visualize. The function maps the epicenters (LATITUDE/LONGITUDE) and annotates each point with in pop up window containing annotation data stored in a column of the data frame.
#'
#'
#' @param data a data frame. Use earthquakes or download dataset from NOAA website.
#' @param annot_col The name of the column from the data to be use for annotation. Default = date.
#'
#' @details Use \code{earthquakes} dataframe from this package or download new dataset from NOAA site.
#'
#' @return A map of the earthquakes epicenters and specified annotations.
#' @export
#'
#' @examples
#' \dontrun{
#' earthquakes %>%
#'   dplyr::filter(country == "MEXICO" & lubridate::year(date) >= 2000) %>%
#'   eq_map(annot_col = "date")
#' }
eq_map <- function(data, annot_col = "date") {
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      lng = data$longitude,
      lat = data$latitude,
      radius = as.numeric(data$mag),
      popup = data[[annot_col]],
      stroke = FALSE,
      fillOpacity = 0.5
    )
}

#' Map HTML labels
#'
#' Takes the dataset as an argument and creates an HTML label that can be used as the annotation text in the leaflet map.
#'
#' @param data a data frame. Use earthquakes or download dataset from NOAA website.
#'
#' @return An HTML label to be used as annotation text in the leaflet map.
#' @export
#'
#' @examples
#' \dontrun{
#' earthquakes %>%
#'   dplyr::filter(country == "MEXICO" & lubridate::year(date) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#' }
#'
eq_create_label <- function(data) {
  paste(ifelse(
    is.na(data$location_name),
    "",
    paste("<b>Location: </b>", data$location_name, "<br/>")
  ),
  ifelse(
    is.na(data$mag),
    "",
    paste("<b>Magnitude: </b>", data$mag, "<br/>")
  ),
  ifelse(
    is.na(data$total_deaths),
    "",
    paste("<b>Total deaths: </b>", data$total_deaths, "<br/>")
  ))
}


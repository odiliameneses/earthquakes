% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq_map.R
\name{eq_map}
\alias{eq_map}
\title{Map earthquake epicenters with annotations}
\usage{
eq_map(data, annot_col = "date")
}
\arguments{
\item{data}{a data frame. Use earthquakes or download dataset from NOAA website.}

\item{annot_col}{The name of the column from the data to be use for annotation. Default = date.}
}
\value{
A map of the earthquakes epicenters and specified annotations.
}
\description{
eq_map() that takes an argument data containing the filtered data frame with earthquakes to visualize. The function maps the epicenters (LATITUDE/LONGITUDE) and annotates each point with in pop up window containing annotation data stored in a column of the data frame.
}
\details{
Use \code{earthquakes} dataframe from this package or download new dataset from NOAA site.
}
\examples{
\dontrun{
earthquakes \%>\%
  dplyr::filter(country == "MEXICO" & lubridate::year(date) >= 2000) \%>\%
  eq_map(annot_col = "date")
}
}

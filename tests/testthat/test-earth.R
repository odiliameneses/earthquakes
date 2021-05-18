#' Tests for each function created in this package.
#' I admit this is not the best test :(

#' Import raw data
filename <- system.file("extdata","earthquakes.tsv",package="earth")
data <- readr::read_delim(filename, delim = "\t")

#' Apply functions to clean data
clean_data <- data %>%
  eq_location_clean() %>%
  eq_clean_data()

# Test eq_clean_data function-------------
testthat::expect_s3_class(clean_data$date,"Date")
testthat::expect_type(clean_data$latitude,"double")
testthat::expect_type(clean_data$longitude,"double")
testthat::expect_type(clean_data$mag,"double")
testthat::expect_type(clean_data$total_deaths,"double")

# Test eq_location_clean function-------------
testthat::expect_s3_class(clean_data, "data.frame")
testthat::expect_false(":" %in% clean_data$location_name)
testthat::expect_false(";" %in% clean_data$location_name)

# Test geom_timeline function-------------
testthat::expect_s3_class(clean_data %>%
                            dplyr::filter(country == "MEXICO" & lubridate::year(date) >= 2000) %>%
                            ggplot2::ggplot(ggplot2::aes(date,location_name,col=total_deaths,size=mag))+
                            geom_timeline(), "ggplot")

# Test geom_timelinelabel function-------------

testthat::expect_s3_class(clean_data %>%
                            dplyr::filter(country == "MEXICO" & lubridate::year(date) >= 2000) %>%
                            ggplot2::ggplot(ggplot2::aes(date,location_name,col=total_deaths,size=mag))+
                            geom_timelinelabel(), "ggplot")



# Test eq_map function-------------
testthat::expect_s3_class(clean_data %>%
                        dplyr::filter(country == "MEXICO" & lubridate::year(date) >= 2000) %>%
                        eq_map(annot_col = "date"), "leaflet")


# Test eq_create_label function-------------
testthat::expect_type(eq_create_label(clean_data), "character")

testthat::expect_s3_class(clean_data %>%
                        dplyr::filter(country == "MEXICO" & lubridate::year(date) >= 2000) %>%
                        dplyr::mutate(popup_text = eq_create_label(.)) %>%
                          eq_map(annot_col = 'popup_text'), "leaflet")

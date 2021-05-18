## code to prepare `earthquakes` dataset goes here


library(magrittr)
raw_data <- readr::read_delim("data-raw/earthquakes.tsv", "\t") %>%
  readr::write_csv(file.path("data-raw", "raw_data.csv"))


earthquakes <- raw_data %>%
  eq_location_clean() %>%
  eq_clean_data() %>%
  dplyr::select(date,
                country,
                area,
                region,
                location_name,
                latitude,
                longitude,
                mag,
                total_deaths)

save(earthquakes, file = "data-raw/earthquakes.rda")

usethis::use_data(earthquakes, overwrite = TRUE)

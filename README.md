# earthquakes

[![Build Status](https://travis-ci.com/odiliameneses/earthquakes.svg?branch=main)](https://travis-ci.com/odiliameneses/earthquakes)

earth is a Data Analysis and Visualization R package built as part of the "Mastering Software Development in R Specialization" by Johns Hopkins University on Coursera: "Mastering Software Development in R Capstone".

This package was was build be used to work with the NOAA Significant Earthquakes dataset and, as part of the MSDR Capstone project, my job is to provide the tools for processing and visualizing the data so that others may extract some use out of the information embedded within.

**In this package you will find:**

**2 functions** to clean NOAA dataset: 

* **`eq_clean_data()`** takes raw NOAA data frame and returns a clean data frame with:  
    + A date column created by uniting the year, month, day and converting it to the Date class.  
    + LATITUDE and LONGITUDE columns converted to numeric class.
    
* **`eq_location_clean()`** that cleans the LOCATION_NAME column by stripping out the country name (including the colon) and converts names to title case (as opposed to all caps).

**2 geoms** that can be used in conjunction with the ggplot2 package to visualize some of the information in the NOAA earthquakes dataset:  

* **`geom_timeline()`** for plotting a time line of earthquakes ranging from xmin to xmaxdates with a point for each earthquake.

* **`geom_timeline_label()`** for adding annotations to the earthquake data. This geom adds a vertical line to each data point with a text annotation (e.g. the location of the earthquake) attached to each line.

**2 functions** tools for mapping the earthquake epicenters and providing some annotations with the mapped data:  

* **`eq_map()`** that takes an argument data containing the filtered data frame with earthquakes to visualize. The function maps the epicenters (LATITUDE/LONGITUDE) and annotates each point with in pop up window containing annotation data stored in a column of the data frame.

* **`geom_timeline_label()`** that takes the dataset as an argument and creates an HTML label that can be used as the annotation text in the leaflet map.

**Ready for a demonstration?**  


### load the package
```{r setup}
library(earth)
```

### upload the data
Download the data from the NOAA website, saved it on your working directory and transform it to a data frame using the `read_delim` function from `readr`.  
In this example, we will use the data saved on this package. It includes data from 1900 until April 2021.
  
```{r eval = FALSE}
filename <- system.file("extdata", "earthquakes.tsv", package="earth")


library(readr)
raw_data <- readr::read_delim(filename, delim = "\t")
```

### Clean the data
Apply functions `eq_clean_data` and `eq_location_clean` to get cleaned data.

```{r eval = FALSE}
cleaned_data <- raw_data %>% 
  eq_location_clean() %>% 
  eq_clean_data()
```

### BONUS: Use `earthquakes` data!
In this package you will find earthquakes data already cleaned.  
Check the documentation for details.


### Visualize the times at which earthquakes occured in Portugal
'Portugal? 'Cause I am Portuguese and why not?!  

And don't forget to load library `ggplot2`!
```{r eval = TRUE}
library(ggplot2)

earthquakes %>%
  dplyr::filter(grepl("PORTUGAL",country) & lubridate::year(date) >= 1900) %>%
  ggplot(aes(x=date,y=location_name,color=total_deaths,size=mag)) +
  geom_timeline(alpha=.4) +
  theme_minimal()+
  theme(legend.position="bottom", legend.box="horizontal", plot.title= element_text(hjust=0.5)) +
  ggtitle("Earthquakes in Portugal since 1900") +
  labs(size = "Richter scale value", color = "# deaths")
```
![image](https://user-images.githubusercontent.com/63351104/119217896-a09b4080-badd-11eb-8a94-52ebf15a71dd.png)

### Add annotations and subset the n_max largest earthquakes by magnitude

```{r eval = TRUE}
earthquakes %>%
  dplyr::filter(grepl("PORTUGAL",country) & lubridate::year(date) >= 1900) %>%
  ggplot(aes(x=date,y=country,color=total_deaths,size=mag)) +
  geom_timeline(alpha=.5) +
  geom_timelinelabel(aes(label=location_name),n_max=3) +
  theme_minimal()+
  theme(legend.position="bottom", legend.box="horizontal", plot.title=element_text(hjust=0.5)) +
  ggtitle("Earthquakes in Portugal since 1900") +
  labs(size = "Richter scale value", color = "# deaths")
```
![image](https://user-images.githubusercontent.com/63351104/119217921-c7f20d80-badd-11eb-9da8-e64f0ed9cf4d.png)

### Map the earthquakes epicenters in Portugal since 1900
 
```{r eval = TRUE}
earthquakes %>%
  dplyr::filter(grepl("PORTUGAL",country) & lubridate::year(date) >= 1900) %>%
  eq_map(annot_col = "date")
```
![image](https://user-images.githubusercontent.com/63351104/119217949-fa036f80-badd-11eb-8084-58a6c9b68885.png)

### Add labels with more info

Use the `eq_create_label` function before the `eq_map` function to add "Location", "Total deaths", and "Magnitude" of the earthquake to the labels.:

```{r eval = TRUE}
earthquakes %>%
  dplyr::filter(grepl("PORTUGAL",country) & lubridate::year(date) >= 1900) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>% 
  eq_map(annot_col = "popup_text")
```
![image](https://user-images.githubusercontent.com/63351104/119217965-130c2080-bade-11eb-85e6-9b18217851f9.png)

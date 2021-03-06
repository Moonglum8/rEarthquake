# rEarthquake package

[![Build Status](https://travis-ci.org/Moonglum8/rEarthquake.svg?branch=master)](https://travis-ci.org/Moonglum8/rEarthquake)

The functions provided by this package use data from the [U.S. National Oceanographic and Atmospheric Administration (NOAA)](https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1) Significant Earthquake Database. This dataset contains information about 5,933 earthquakes over an approximately 4,000 year time span.

## Required set-up for this package

Currently, this package exists in a development version on GitHub. To use the package, you need to install it directly from GitHub using the `install_github` function from `devtools`. 

You can use the following code to install the development version of `rEarthquake`: 

```R
library(devtools)
install_github("moonglum8/rEarthquake")
library(rEarthquake)
```

In addition the following libraries are required:

```R
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(grid)
library(ggplot2)
library(leaflet)
```

## Sample data included with the package

Users may access a sample dataframe by running the code below. This has details of earthquakes from around 
4000 BCE to April 2017.

```R
eq_data_raw
```

The raw data file for this data frame is in tab delimited format can be accessed (and saved to your currrent working directory) by running the following:

```R
library(readr)
data_raw <-system.file("extdata", "signif.txt", package = "rEarthquake")
file.copy(from=c(data_raw),to=getwd())
eq_data_raw_wd <- readr::read_tsv("./signif.txt")
```
## rEarthquake

The user has access to a number of functions/geoms to help tidy and visualise the data. These are: 

* Data tidying functions:
     + `eq_clean_data`
     + `eq_location_clean`
* Geom for creating visualisations:
     + `geom_timeline`
     + `geom_timeline_label`
* Helper functions to create visualisations:
     + `get_timeline`
     + `get_timeline_label`
* Mapping functions
     + `eq_map`
     + `eq_create_label`

Use of these functions are shown below.

### eq_clean_data

Takes the raw data set and adds new columns "date", "longitude" and "latitude".

```R
eq_data_cleaned <- eq_clean_data(eq_data_raw)
```
### eq_location_clean

Takes the raw data set and modified the column LOCATION_NAME to strip out country names and reformats to
title case. This is recommended before passing the data into the "_label" functions to improve presentation
of the output. The function can be used in conjuntion with `eq_clean_data` either before or after it in a %>% chain.

```R
eq_clean_data(eq_data_raw) %>% eq_location_clean()
````
### geom_timeline

A ggplot2 graphical function to plot a timeline of earthquakes from cleaned data. The plot indicates the magnitude of each earthquake and number of deaths.

```R
eq_clean_data(eq_data_raw) %>%
     dplyr::filter(COUNTRY %in% c("USA","IRAN")  %>%
     ggplot2::ggplot() +
     geom_timeline(aes(x=date,y=COUNTRY,colour=DEATHS, size=EQ_PRIMARY, fill=DEATHS, 
          xmin = lubridate::ymd("2000-01-01"), xmax = lubridate::ymd("2016-01-01")))
```

### geom_timeline_label

A ggplot2 graphical function that adds labels to earthquakes visualised. There is an option to select the "n" largest earthquakes by magnitude to which to apply the labels. Best used with `eq_location_clean`.

```R
eq_clean_data(eq_data_raw) %>% eq_location_clean() %>%
     dplyr::filter(COUNTRY %in% c("USA","IRAN"))  %>%
     ggplot2::ggplot() +
     geom_timeline(aes(x = date,
          y = COUNTRY,
          colour = DEATHS, 
          size = EQ_PRIMARY, 
          fill = DEATHS, 
          xmin = lubridate::ymd("2000-01-01"), 
          xmax = lubridate::ymd("2016-01-01"))) +
     geom_timeline_label(aes(x = date,
          location = LOCATION_NAME,
          xmin = lubridate::ymd("2000-01-01"), 
          xmax = lubridate::ymd("2016-01-01"),
          size=EQ_PRIMARY,n_max=5,y=COUNTRY)) 
```

### get_timeline

A wrapper function to help generate timeline visualisations easier.

```R
get_timeline(eq_data_raw, c("USA","IRAN"),"1970-01-01","2016-01-01")
```

### get_timeline_label

A wrapper function to help generate timeline (with labels) visualisations easier.

```R
get_timeline_label(eq_data_raw, c("USA","CHINA"),"2010-01-01","2016-01-01", n_max = 5)
```

### eq_map

A function to generate an interactive map showing earthquakes for a particular country. The user specifies a column from the data which the earthquake is to be annotated by eg date.

```R
eq_clean_data(eq_data_raw) %>% 
     dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>% 
     eq_map(annot_col="date")
```

### eq_create_label

A function to generate a custom popup box for a selected earthquake showing location, magnitude and total deaths.

```R
eq_clean_data(eq_data_raw) %>% 
     eq_location_clean() %>% 
     dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>% 
     dplyr::mutate(popup_text = eq_create_label(.)) %>% 
     eq_map(annot_col="popup_text")
```

## Miscellaneous

The package also contains a new theme `theme_timeline` for use with `geom_timeline` and `geom_timeline_label`. It is applied automatically when using `get_timeline` and `get_timeline_label`.

There are two custom grobs and a custom stat included with the package which are used by the `geom_` functions. These are `geomTimeline`, `geomTimelineLabel` and `StatTimeline`.

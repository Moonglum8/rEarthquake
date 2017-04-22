library(rEarthquake)

## eq_clean_data tests
test_data_clean <- eq_clean_data(eq_data_raw)

test_that("eq_clean_data has correct classes", {
     expect_is(test_data_clean,"data.frame")
     expect_is(test_data_clean$date,"POSIXct")
     expect_is(test_data_clean$longitude,"numeric")
     expect_is(test_data_clean$latitude,"numeric")
})

## eq_location_clean_data tests
test_data_clean_loc <- eq_clean_data(eq_data_raw)

test_that("eq_location_clean has correct classes", {
     expect_is(test_data_clean_loc,"data.frame")
     expect_is(test_data_clean_loc$LOCATION_NAME,"character")
})

## eq_create_label tests
library(dplyr)
library(lubridate)
test_data_annote <- test_data_clean %>%
     eq_location_clean() %>%
     dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>%
     dplyr::mutate(popup_text = eq_create_label(.))

test_that("eq_create_label has correct classes", {
     expect_is(test_data_annote,"data.frame")
     expect_is(test_data_annote$popup_text,"character")
})

## eq_map tests
library(leaflet)
test_map <- test_data_annote %>% eq_map(annot_col="popup_text")

test_that("eq_map has correct class", {
     expect_is(test_map ,"leaflet")
})

## geom_timeline test
library(ggplot2)
library(grid)
test_geom_timeline <- geom_timeline()
test_that("geom_timeline has correct class", {
     expect_is(test_geom_timeline ,"ggproto")
})

## geom_timeline_label test
test_geom_timeline_label <- geom_timeline_label()
test_that("geom_timeline_label has correct class", {
     expect_is(test_geom_timeline_label ,"ggproto")
})

## get_timeline test
test_get_timeline <- get_timeline(eq_data_raw, c("USA","IRAN"),"1970-01-01","2016-01-01")
test_that("get_timeline has correct class", {
     expect_is(test_get_timeline ,"ggplot")
})

## get_timeline_label test
test_get_timeline_label <- get_timeline_label(eq_data_raw, c("USA","CHINA"),"2010-01-01","2016-01-01", n_max = 5)
test_that("get_timeline_label has correct class", {
     expect_is(test_get_timeline_label ,"ggplot")
})

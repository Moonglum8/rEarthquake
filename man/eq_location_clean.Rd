% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/earthquake.R
\name{eq_location_clean}
\alias{eq_location_clean}
\title{eq_location_clean}
\usage{
eq_location_clean(eq_data)
}
\arguments{
\item{eq_data}{A data table containing NOAA Earthquake data}
}
\value{
A date frame with LOCATION_NAME cleaned to have country names removed and text in title case.
}
\description{
Takes the raw data set and modified the column LOCATION_NAME to strip out country names and reformats to
title case. This is recommended before passing the data into the "_label" functions to improve presentation
of the output. The function can be used in conjuntion with `eq_clean_data` either before or after it in a %>% chain.
}
\details{
A regular expression is used to match and remove the country names.
}
\examples{
library(dplyr)
USA_clean_loc <- eq_clean_data(eq_data_raw) \%>\%
  eq_location_clean() \%>\% dplyr::filter(COUNTRY \%in\% "USA")
USA_IRAN_clean_loc <- eq_clean_data(eq_data_raw) \%>\%
   eq_location_clean() \%>\% dplyr::filter(COUNTRY \%in\% c("USA","IRAN"))

}

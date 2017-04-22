library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(grid)
library(ggplot2)
library(leaflet)
library(devtools)
library(roxygen2)

#######################################  module 1 ##########################################################

## Earthquate data (in tab delimited format) obtained from
## https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1

## read in tab separated file
## eq_data_raw <- readr::read_tsv("./data/signif.txt")

#' eq_clean_data
#'
#' Takes the raw data set and adds new columns "date", "longitude" and "latitude".
#'
#' @param eq_data A data table containing NOAA Earthquake data
#' @return A date frame with a new column called date (in POSIXct format) and longitude and latitude
#' columns formated as numeric.
#' @export
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom tidyr unite
#' @importFrom stringr str_pad
#' @importFrom lubridate parse_date_time
#' @importFrom lubridate years
#'
#' @examples
#' library(dplyr)
#' USA <- eq_clean_data(eq_data_raw) %>% dplyr::filter(COUNTRY %in% "USA")
#' USA_IRAN <- eq_clean_data(eq_data_raw) %>%
#'   dplyr::filter(COUNTRY %in% c("USA","IRAN"))
#'
eq_clean_data <- function(eq_data) {

     # need to deal with dates BC (ie megative years)
     # to do this will subtract YEAR from "0000-01-01"
     # will assume that earthquake occured at start of year if missing MONTH

     s1 <- eq_data %>%
          ## need to pad any years to ensure that we have YYYY format
          ## complicated by BCE years (negative years) set these to "0000" for now and adjust later
          ## use bcyears to store number to adjust BCE years by (will either be negative [if BCE] or zero [if AD])
          dplyr::mutate(year = ifelse(YEAR <0, "0000",
                                      stringr::str_pad(as.character(YEAR),4,"left","0")),
                        bcyear = ifelse(YEAR <0, YEAR , 0)
          ) %>%
          ## unite our AD years with month and day
          tidyr::unite(datetime,year,MONTH,DAY, remove = FALSE) %>%
          ## s1 date finds the AD date
          ## set long and lat to numeric
          dplyr::mutate(date_s1 = lubridate::parse_date_time(datetime, "Ymd", truncated = 2),
                        longitude = as.numeric(LONGITUDE),
                        latitude = as.numeric(LATITUDE)) %>%
          ## adjust s1 date by bcyears to find our formatted date
          dplyr::mutate(date = date_s1 + lubridate::years(bcyear)) %>%
          ## remove the intermediate columns we have used
          dplyr::select(-date_s1,-bcyear,-year,-datetime)

     ## return the cleaned data
     return(s1)
}

#' eq_location_clean
#'
#' Takes the raw data set and modified the column LOCATION_NAME to strip out country names and reformats to
#' title case. This is recommended before passing the data into the "_label" functions to improve presentation
#' of the output. The function can be used in conjuntion with `eq_clean_data` either before or after it in a %>% chain.
#'
#' @param eq_data A data table containing NOAA Earthquake data
#'
#' @return A date frame with LOCATION_NAME cleaned to have country names removed and text in title case.
#' @details A regular expression is used to match and remove the country names.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom stringr str_to_title
#' @export
#'
#' @examples
#' library(dplyr)
#' USA_clean_loc <- eq_clean_data(eq_data_raw) %>%
#'   eq_location_clean() %>% dplyr::filter(COUNTRY %in% "USA")
#' USA_IRAN_clean_loc <- eq_clean_data(eq_data_raw) %>%
#'    eq_location_clean() %>% dplyr::filter(COUNTRY %in% c("USA","IRAN"))
#'
eq_location_clean <- function(eq_data) {
     eq_data %>%
          ## use regex to identify counties to strip out and convert to title case
          dplyr::mutate(LOCATION_NAME = stringr::str_to_title(base::gsub("[^;\n]+[:]","",LOCATION_NAME)))
}


#######################################  module 2 ##########################################################

#' geom_timeline
#'
#' A ggplot2 graphical function to plot a timeline of earthquakes from cleaned data.
#' The plot indicates the magnitude of each earthquake and number of deaths.
#'
#' @section Aesthetics:
#' \code{geom_timeline} understands the following aesthetics:
#' \itemize{
#'   \item \code{x} date
#'   \item \code{y} latitude
#'   \item \code{xmin} minimum date for earthquakes
#'   \item \code{xmax} maximum date for earthquakes
#'   \item \code{size} used to size shape based on magnitude of earthquake eg EQ_PRIMARY
#'   \item \code{fill} used to colour shape based on number of deaths eg DEATHS
#'   \item \code{colour} used to colour shape based on number of deaths eg DEATHS
##' }
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#' @param ... ...
#'
#' @return ggplot2 graphical object
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(lubridate)
#' eq_clean_data(eq_data_raw) %>% eq_location_clean() %>%
#' dplyr::filter(COUNTRY %in% c("USA","IRAN"))  %>%
#'     ggplot2::ggplot() +
#'     geom_timeline(aes(x = date,
#'                       y = COUNTRY,
#'                       colour = DEATHS,
#'                       size = EQ_PRIMARY,
#'                       fill = DEATHS,
#'                       xmin = lubridate::ymd_hm("2000-01-01",truncated = 2),
#'                       xmax = lubridate::ymd_hm("2016-01-01",truncated = 2)))
#'
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
     ggplot2::layer(
          stat = StatTimeline, geom = geomTimeline, mapping = mapping,
          data = data,  position = position,
          show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, ...)
     )
}

#' geomTimeline
#' @rdname Earthquake-ggproto
#' @format NULL
#' @usage NULL
# @importFrom grid segmentGrob
# @importFrom grid pointsGrob
# @importFrom grid xaxisGrob
# @importFrom grid gTree
# @importFrom grid gList
#' @export
geomTimeline <- ggplot2::ggproto("geomTimeline", Geom,
        required_aes = c("x"),
        optional_aes = c("y", "xmin","xmax"),
        default_aes = aes(shape = 21, size = 1, colour = "blue", fill = "blue", alpha = 0.5, stroke = 1, y = 0.5),
        draw_key = draw_key_point,

        draw_group = function(data, panel_scales, coord) {
              #  browser()
             coords <- coord$transform(data, panel_scales)

             # SegmentGrob to draw line where we will plot our earthquake points
             seg_grob <- grid::segmentsGrob(
                  x0 = unit(coords$xmin,"native"),
                  x1 = unit(coords$xmax,"native"),
                  y0 = unit(coords$y,"native"),
                  y1 = unit(coords$y,"native"),
                  gp = grid::gpar(col = "grey", alpha = 0.25)
             )
             # pointGrob to draw earthquakes with varying size and alpha
             point_grob <- grid::pointsGrob(
                  x = unit(coords$x,"native"),
                  y = coords$y,
                  pch = coords$shape,
                  size = unit(coords$size,"mm"),
                  gp = grid::gpar(col = coords$colour, fill = coords$fill, alpha = coords$alpha)
             )
             # draws an xaxis
             axis_grob <- grid::xaxisGrob()
             # group our grobs together for output
             grid::gTree(children = grid::gList(seg_grob,point_grob,axis_grob))
        })

#' StatTimeline
#' @rdname Earthquake-ggproto
#' @format NULL
#' @usage NULL
# @importFrom dplyr filter
#' @export
StatTimeline <- ggproto("StatTimeline", Stat,
                   required_aes = c("x","xmin","xmax"),
                  # default_aes = aes(y = ..density..),
                   setup_params = function(data, params) {
                        min <- data$xmin
                        max <- data$xmax
                        list(
                             min = min,
                             max = max,
                             na.rm = params$na.rm
                        )
                   },
              # want to filter on min and max dates and also remove any NA's from size
                   compute_group = function(data, scales, min, max) {
                        data %>% dplyr::filter(data$x > data$xmin & data$x < data$xmax & !is.na(data$size))
                   }
               )

#' theme_timeline
#' @rdname Earthquake-theme
#' @format NULL
#' @usage NULL
#' @export
theme_timeline <- theme_classic() + theme(axis.title.x = element_text(face = "bold"),
                                          axis.line.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank(),
                                          legend.box = "horizontal",
                                          legend.direction = "horizontal",
                                          legend.position = "bottom")


#' get_timeline
#'
#' A wrapper function to help generate timeline visualisations easier.
#'
#' @param data_raw A data table containing NOAA Earthquake data
#' @param clist Character/Vector of grouping names eg "USA" from COUNTRY column.
#' @param xmin POSIXct date - minimum date for timeline
#' @param xmax POSIXct date - maximum date for timeline
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 scale_size_continuous
#' @importFrom ggplot2 scale_fill_continuous
#' @importFrom ggplot2 scale_colour_continuous
#' @importFrom ggplot2 scale_alpha_continuous
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 aes
#' @importFrom lubridate ymd_hm
#'
#' @return A ggplot2 graphical object displaying timeline of earthquakes data
#' @export
#'
#' @examples
#' \dontrun{
#' get_timeline(eq_data_raw, c("USA","IRAN"),"1970-01-01","2016-01-01")
#' }
get_timeline <- function(data_raw, clist = "ALL", xmin, xmax) {
     xmin <- lubridate::ymd_hm(xmin,truncated = 2)
     xmax <- lubridate::ymd_hm(xmax,truncated = 2)
     if (!(clist[1] == "ALL")) {
          eq_clean_data(data_raw) %>%
               dplyr::filter(COUNTRY %in% clist)  %>%
               ggplot2::ggplot() +
               geom_timeline(ggplot2::aes(x=date,y=COUNTRY,colour=DEATHS, size=EQ_PRIMARY, fill=DEATHS, xmin = xmin, xmax = xmax)) +
               ggplot2::scale_size_continuous(name = "Richter scale values") +
               ggplot2::scale_fill_continuous(name = "# Deaths") +
               ggplot2::scale_colour_continuous(name = "# Deaths") +
               ggplot2::scale_alpha_continuous(name = "# Deaths")  +
               theme_timeline +
               ggplot2::xlab("DATE")
     } else {
          eq_clean_data(data_raw) %>%
               ggplot2::ggplot() +
               geom_timeline(ggplot2::aes(x=date,colour=DEATHS, size=EQ_PRIMARY, fill=DEATHS, xmin = xmin, xmax = xmax)) +
               ggplot2::scale_size_continuous(name = "Richter scale values") +
               ggplot2::scale_fill_continuous(name = "# Deaths") +
               ggplot2::scale_colour_continuous(name = "# Deaths") +
               ggplot2::scale_alpha_continuous(name = "# Deaths") +
               theme_timeline +
               ggplot2::xlab("DATE")
     }
}


#' geom_timeline_label
#'
#' A ggplot2 graphical function that adds labels to earthquakes visualised.
#' There is an option to select the "n" largest earthquakes by magnitude to which to apply the labels.
#' Best used with `eq_location_clean`.
#'
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#' @param ... ...
#'
#' @section Aesthetics:
#' \code{geom_timeline_label} understands the following aesthetics:
#' \itemize{
#'   \item \code{x} date
#'   \item \code{y} (optional) aes can be used to group output eg by COUNTRY
#'   \item \code{location} aes used to selection labels eg LOCATION_NAME
#'   \item \code{xmin} minimum date for earthquakes
#'   \item \code{xmax} maximum date for earthquakes
#'   \item \code{size} aes used to indicate size eg EQ_PRIMARY
#'   \item \code{n_max} the top n number of labels to show based on size aes, defaults to n = 5
#' }
#'
#' @return A ggplot2 graphical object for labelling plots generated with geom_timeline.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(lubridate)
#' eq_clean_data(eq_data_raw) %>% eq_location_clean() %>%
#' dplyr::filter(COUNTRY %in% c("USA","IRAN"))  %>%
#'     ggplot2::ggplot() +
#'     geom_timeline(aes(x = date,
#'                       y = COUNTRY,
#'                       colour = DEATHS,
#'                       size = EQ_PRIMARY,
#'                       fill = DEATHS,
#'                       xmin = lubridate::ymd_hm("2000-01-01",truncated=2),
#'                       xmax = lubridate::ymd_hm("2016-01-01",truncated=2))) +
#'     geom_timeline_label(aes(x = date,
#'                             location = LOCATION_NAME,
#'                             xmin = lubridate::ymd_hm("2000-01-01",truncated=2),
#'                             xmax = lubridate::ymd_hm("2016-01-01",truncated=2),
#'                             size=EQ_PRIMARY,n_max=5,y=COUNTRY))
#'
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
     ggplot2::layer(
          geom = geomTimelineLabel, stat = StatTimeline, mapping = mapping,
          data = data,  position = position,
          show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, ...)
     )
}

#' geomTimelineLabel
#' @rdname Earthquake-ggproto
#' @format NULL
#' @usage NULL
# @importFrom grid segmentGrob
# @importFrom grid textGrob
# @importFrom grid gTree
# @importFrom grid gList
#' @export
geomTimelineLabel <- ggplot2::ggproto("geomTimelineLable", Geom,
             required_aes = c("x","location"),
             optional_aes = c("y","n_max"),
             default_aes = aes(size =0, y = 0.5, fontsize = 8, alpha = 0.75, colour = "blue", fill = "blue"),
             draw_key = draw_key_blank,

             draw_panel = function(data, panel_scales, coord) {
                  data
                  # browser()
                  if ("n_max" %in% names(data)) {
                         nm <-  data$n_max[1]
                  } else {
                         nm <- 0
                  }
                  if ("n_max" %in% names(data) & nrow(data) > nm) {
                       data <- data %>%
                            dplyr::group_by(y) %>%
                            dplyr::top_n(n = data$n_max[1], wt = size)
                       data
                  }

                  coords <- coord$transform(data, panel_scales)

                  # SegmentGrob to draw lines where we will plot our earthquake points
                  seg_grob <- grid::segmentsGrob(
                       x0 = unit(coords$x,"native"),
                       x1 = unit(coords$x,"native"),
                       y0 = unit(coords$y,"native"),
                       y1 = unit(coords$y + 0.05,"native"),
                       gp = grid::gpar(col = "grey", alpha = 0.75)
                  )
                  # textGrob to print location
                  text_grob <- grid::textGrob(
                       label = coords$location,
                       x = unit(coords$x,"native"),
                       y = unit(coords$y + 0.06,"native"),
                       rot = 45,
                       just = "left",
                       gp = grid::gpar(fontsize = 8)
                  )
                  # group our grobs together for output
                  grid::gTree(children = grid::gList(seg_grob,text_grob))
             })

#' get_timeline_label
#'
#' A wrapper function to help generate timeline (with labels) visualisations easier.
#'
#' @param data_raw A data table containing NOAA Earthquake data
#' @param clist Character/Vector of grouping names eg "USA" from COUNTRY column.
#' @param xmin POSIXct date - minimum date for timeline
#' @param xmax POSIXct date - maximum date for timeline
#' @param n_max Integer value to control number of labels per group to show
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 scale_size_continuous
#' @importFrom ggplot2 scale_fill_continuous
#' @importFrom ggplot2 scale_colour_continuous
#' @importFrom ggplot2 scale_alpha_continuous
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 aes
#' @importFrom lubridate ymd_hm
#'
#' @return A ggplot2 graphical object displying timeline earthquate data with labels.
#' @export
#'
#' @examples
#' \dontrun{
#' get_timeline_label(eq_data_raw, c("USA","CHINA"),"2010-01-01","2016-01-01", n_max = 5)
#' }
get_timeline_label <- function(data_raw, clist = "ALL", xmin, xmax, n_max = 5) {
     #browser()
     xmin <- lubridate::ymd_hm(xmin,truncated = 2)
     xmax <- lubridate::ymd_hm(xmax,truncated = 2)
     if (!(clist[1] == "ALL")) {
          eq_clean_data(data_raw) %>% eq_location_clean() %>%
               dplyr::filter(COUNTRY %in% clist)  %>%
               ggplot2::ggplot() +
               geom_timeline(ggplot2::aes(x=date,y=COUNTRY,colour=DEATHS, size=EQ_PRIMARY, fill=DEATHS, xmin = xmin, xmax = xmax)) +
               geom_timeline_label(ggplot2::aes(x=date, location=LOCATION_NAME,xmin=xmin,xmax=xmax,size=EQ_PRIMARY,n_max=n_max,y=COUNTRY)) +
               ggplot2::scale_size_continuous(name = "Richter scale values") +
               ggplot2::scale_fill_continuous(name = "# Deaths") +
               ggplot2::scale_colour_continuous(name = "# Deaths") +
               ggplot2::scale_alpha_continuous(name = "# Deaths")  +
               theme_timeline +
               ggplot2::xlab("DATE")
     } else {
          eq_clean_data(data_raw) %>% eq_location_clean() %>%
               ggplot2::ggplot() +
               geom_timeline(ggplot2::aes(x=date,colour=DEATHS, size=EQ_PRIMARY, fill=DEATHS, xmin = xmin, xmax = xmax)) +
               geom_timeline_label(ggplot2::aes(x=date, location=LOCATION_NAME,xmin=xmin,xmax=xmax,size=EQ_PRIMARY,n_max=n_max,y=NULL)) +
               ggplot2::scale_size_continuous(name = "Richter scale values") +
               ggplot2::scale_fill_continuous(name = "# Deaths") +
               ggplot2::scale_colour_continuous(name = "# Deaths") +
               ggplot2::scale_alpha_continuous(name = "# Deaths") +
               theme_timeline +
               ggplot2::xlab("DATE")
     }
}

#######################################  module 3 ##########################################################


#' eq_map
#'
#' A function to generate an interactive map showing earthquakes for a particular country.
#' The user specifies a column from the data which the earthquake is to be annotated by eg date.
#'
#' @param eq_data A data table containing NOAA Earthquake data
#' @param annot_col A column found in \code{eq_data} to annotate earthquake marker
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#'
#' @return An interactive map displaying earthquate location for a given country with user defined popup.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#' eq_clean_data(eq_data_raw) %>%
#'    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>% eq_map(annot_col="date")
eq_map <- function(eq_data, annot_col) {

     leaflet::leaflet() %>% leaflet::addTiles() %>% leaflet::addCircleMarkers(data=eq_data,
                                                   lng = ~ longitude,
                                                   lat = ~latitude,
                                                   radius = ~EQ_PRIMARY,
                                                   popup = ~ eq_data[[annot_col]])
}

#' eq_create_label
#'
#' A function to generate a custom popup box for a selected earthquake showing location,
#' magnitude and total deaths.
#'
#' @param eq_data A data table containing NOAA Earthquake data
#'
#' @return An interactive map displaying earthquate location for a given country with custom popup.
#' @export
#'
#' @examples
#' library(dplyr)
#' eq_clean_data(eq_data_raw) %>%
#'   eq_location_clean() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>% eq_map(annot_col="popup_text")
#'
eq_create_label <- function(eq_data) {
     loc <- ifelse(is.na(eq_data$LOCATION_NAME),"",paste("<b>Location:</b>", eq_data$LOCATION_NAME, "<br />"))
     mag <- ifelse(is.na(eq_data$EQ_PRIMARY),"",paste("<b>Magnitude:</b>", eq_data$EQ_PRIMARY, "<br />"))
     death <- ifelse(is.na(eq_data$TOTAL_DEATHS),"",paste("<b>Total deaths:</b>", eq_data$TOTAL_DEATHS, "<br />"))
     out <- paste(loc,mag,death)
     return(out)
}

# ====================================================
# Author: Carter Vonderahe
# Date: 12/7/23
# Professor Donghyung Lee
# STA 404 Project 2 - New York City Airbnb Listing Explorer
# DATA PROCESSING
# ====================================================


# set working directory - to be changed by whoever runs this code
setwd("/Users/carter_vondy/Desktop/STA404/Projects/Project2")

# load libraries
library(tidyverse)
library(lubridate)


####################
### READ IN DATA ###
####################

# read in airbnb data
airbnb_data <- read.csv("listings.csv")
# read in crime data
crime_data <- read.csv("NYPD_Arrest_Data_2023.csv")
# read in pop data
pop_data <- read.csv("boro_population_by_month_2023.csv")
# read in neighborhoods as sp
ny_neighborhoods <- geojsonio::geojson_read("neighbourhoods.geojson", what = "sp") 


#########################
### DATA MANIPULATION ###
#########################

# ----------------
### MAP VISUAL ###
# ----------------

# creates function to make custom legend
# from this site: https://stackoverflow.com/questions/76871212/add-leaflet-legend-for-polygon-outlines
addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 0.5, position){
  
  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("circle", "50%", shapes)
    shapes <- gsub("square", "0%", shapes)
    paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
  }
  make_labels <- function(sizes, labels) {
    paste0("<div style='display: inline-block;height: ", 
           sizes, "px;margin-top: 0px;line-height: ", 
           sizes, "px;'>", labels, "</div>")
  }
  
  legend_colors <- make_shapes(colors, sizes, borders, shapes)
  legend_labels <- make_labels(sizes, labels)
  
  return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity, position = position))
}


# ---------------
### BAR GRAPH ###
# ---------------

# get ratings from names - 23896 out of 39106 have ratings, the rest are NA
# sum(stringr::str_count(airbnb_data$name, pattern = "★"))
airbnb_data <- airbnb_data %>%
  mutate(rating = str_extract(name, "★[0-9]\\.[0-9]+")) %>%
  mutate(rating = as.numeric(str_extract(rating, "[0-9]\\.[0-9]+")))

# set all NA ratings to 0
airbnb_data$rating[is.na(airbnb_data$rating)] <- 0


# ----------------
### LINE GRAPH ###
# ----------------

# convert ARREST_DATE to date object
crime_data$ARREST_DATE <- mdy(crime_data$ARREST_DATE)

# rename borough levels
crime_data$ARREST_BORO <- recode_factor(crime_data$ARREST_BORO,
                                        B = "Bronx",
                                        K = "Brooklyn",
                                        M = "Manhattan",
                                        Q = "Queens",
                                        S = "Staten Island")

# summarize data by location and month
crime_sum_data <- crime_data %>%
  group_by(ARREST_BORO, month(ARREST_DATE, label = TRUE)) %>%
  summarise(num_arrests = n())

# rename month column to "Month"
names(crime_sum_data)[names(crime_sum_data) == 'month(ARREST_DATE, label = TRUE)'] <- 'Month'

# prepare crime and pop datasets for merging
crime_sum_data2 <- crime_sum_data %>%
  mutate(boro_month = paste(ARREST_BORO, Month, sep = "-"))
pop_data2 <- pop_data %>%
  mutate(boro_month = paste(borough, month, sep = "-"))

# merge crime and pop datasets and calculate arrest rates for each borough
crime_pop_data <- left_join(crime_sum_data2, pop_data2, by = "boro_month") %>%
  mutate(arrest_rate = num_arrests / total * 1000) %>% # arrests per 1000 residents
  mutate(Month = paste(Month, "01", "2023", sep = "-")) %>% # adds a day and year for converting to date
  select(ARREST_BORO, Month, num_arrests, arrest_rate)

# convert month to date object
crime_pop_data$Month <- as.Date(crime_pop_data$Month, format = '%b-%d-%Y')

# make a new Shiny widget type called "dateRangeInput2" so user can only choose between months
# https://stackoverflow.com/questions/31152960/display-only-months-in-daterangeinput-or-dateinput-for-a-shiny-app-r-programmin
dateRangeInput2 <- function(inputId, label, minview = "days", maxview = "decades", ...) {
  d <- shiny::dateRangeInput(inputId, label, ...)
  d$children[[2L]]$children[[1]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$children[[3]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$children[[1]]$attribs[["data-date-max-view-mode"]] <- maxview
  d$children[[2L]]$children[[3]]$attribs[["data-date-max-view-mode"]] <- maxview
  d
}


#############
# SAVE DATA #
#############

save(airbnb_data, crime_pop_data, ny_neighborhoods, addLegendCustom, dateRangeInput2,
     file = "Project2_ProcessedData.RData")


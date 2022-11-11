# this is a scratch file to test new functions etc. locally before moving them to the shiny app

# load libraries
library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)
library(dplyr)

source("functions.R")

# load data (put in a doenetid - good doenetids to use are on slack)
doenetid <- ""
raw <-  stream_in(file(
  paste0(
    "https://www.doenet.org/api/getEventData.php?doenetId[]=",
    doenetid
  )
))

# clean the data, test the functions
events <-  raw$events[[1]]
dates <- pull_dates(events)
versions <- pull_versions(events)
min_date <- min(dates)
max_date <- max(dates)
cleaned_versions <- clean_events(events, min(dates), max(dates))
summary_data_versions <- summarize_events(cleaned_versions)
cleaned <- version_filter(cleaned_versions, 1)
summary_data <- summarize_events(cleaned)

###########################################
##### do local work below here
###########################################




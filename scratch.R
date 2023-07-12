# this is a scratch file to test new functions etc. locally before moving them to the shiny app

# load libraries
library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)
library(dplyr)
library(DT)

source("functions.R")

# load data (put in a doenetid - good doenetids to use are on slack)
#doenetid <- "_TETkqoYS3slQaDwjqkMrX"
doenetid <- "_IJg9jJIA8Ar99yfFETgiG"
raw <-  stream_in(file(
  paste0(
    "https://www.doenet.org/api/getEventData.php?doenetId[]=",
    doenetid
  )
))

# clean the data, test the functions
events <-  raw$events[[1]]
#events <- read.csv("base.csv")
#events <- events[1:100,]
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


# the implementations below are different ways of extracting the ids
# where code1 and code2 are not fully functional with code2 having a possible
# in infinite loop. Code3 works but it needs more urls for testing purposes
library(stringr)

extract_ids_code1 <- function(url) {
  start_index <- str_locate(url, "data=")[,2]
  ids <- list()
  match_length <- attr(regexpr("data=", url), "match.length")
  
  while (start_index > -1) {
    end_index <- str_locate(url, "&data=", start = start_index)[, 2]
    if (end_index == -1) {
      ids <- c(ids, list(str_sub(url, start_index)))
      break
    }
    ids <- c(ids, list(str_sub(url, start_index, end_index - 1)))
    start_index <- end_index + match_length
  }
  
  return(ids)
}
extract_ids_code2 <- function(url) {
  start_index <- regexpr("data=", url)[1] + 5
  ids <- list()
  match_length <- attr(regexpr("data=", url), "match.length")
  
  
  while (start_index > 0) {
    end_index <- regexpr("&data=", url, start_index)[1]
    if (end_index == -1) {
      ids <- c(ids, list(substr(url, 
                                start_index)))
      break
    }
    ids <- c(ids, list(substr(url, start_index, end_index - 1)))
    start_index <- end_index + match_length
  }
  
  return(ids)
}
extract_ids_code3 <- function(url) {
  url_1 <- gsub("&code=.*", "", url)
  url_2 <- sub("https://doenet.shinyapps.io/analyzer/\\?", "", url_1)
  url_3 <- sub("data=", "&data=", url_2)
  
  ids <- as.list(strsplit(url_3, "&data=")[[1]][-1])
  
  return(ids)
}

url <- "https://doenet.shinyapps.io/analyzer/?data=_Y8rhJ0x5KzbEF4cc73RFH&data=_szGjThMMAaq0gmXaig9nq&code=4k6dSxGZ0BSztlexusbmU"
ids <- extract_ids(url)
print(ids)





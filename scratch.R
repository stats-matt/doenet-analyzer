library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)

source("functions.R")

getQueryString()[["data"]]
renderText(getQueryString()[["data"]])


x = stream_in(file("https://www.doenet.org/api/getEventData.php?doenetId[]=_MdyLSpVBP8I7jb7VVNKD1"))
    
x
events = x$events[[1]]
View(events)
View(x)
class(x)
cleaned = clean_events(events)
View(cleaned)
max
class(max)

grouped = cleaned %>% group_by(activityCid)
grouped
View(grouped)

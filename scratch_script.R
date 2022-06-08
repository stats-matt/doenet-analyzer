library(tidyverse)
library(jsonlite)
library(anytime)
library(shiny)
library(tidytext)


x = stream_in(
  file(
    "https://www.doenet.org/api/getEventData.php?doenetId[]=_YImZRcgrUqyNBLHd0tbP2&doenetId[]=_NdFHXQMHdG9iQndzmwFTo"))
class(x)
head(x)
dim(x)
colnames(x)
rownames(x)

events <- x$events[[1]]
View(events)
cleaned = clean_events(events)
View(cleaned)
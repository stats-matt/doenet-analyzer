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

typeof(renderUI({textInput(paste0("id",i), paste0("Deonet ID",i))}))
data.frame(renderUI({textInput(paste0("id",i), paste0("Deonet ID",i))}))
class(renderUI({textInput(paste0("id",i), paste0("Deonet ID",i))}))
x = list(renderUI({textInput(paste0("id",1), paste0("Deonet ID",1))}),renderUI({textInput(paste0("id",2), paste0("Deonet ID",2))}))
x[[1]]
x[[2]]
x[[3]]
x[[4]]
num_inputs = 3
(num_inputs+1):5

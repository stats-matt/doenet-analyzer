library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)
library(dplyr)



raw = stream_in(file(paste0("https://www.doenet.org/api/getEventData.php?doenetId[]=_dKAX4QFX3JGXILGwaApZY")))
unpacked = raw$events[[1]]

#cleaned = clean_events(unpacked)
#summarized = summarize_events(cleaned)
#nrow(distinct(summarized, score))



dates = unpacked %>% select(timestamp)

proc = anytime(dates$timestamp)
class(proc)
proc[1]
min(proc)
max(proc)
max(proc) > min(proc)


cleaned = clean_events(unpacked, min(proc),max(proc))
View(cleaned)
summarized = summarize_events(cleaned)
View(summarized)
summarized = summarized %>% group_by(response)
View(summarized)
interm = summarized %>% group_by(problem) %>% filter(creditAchieved < 1) %>% ggplot(aes(x = response, y = n,fill = as.factor(response))) + geom_col()+facet_wrap(~problem)
plot(interm)


source("functions.R")
numvers = pull_versions(cleaned)
numvers

cleaned_versions <- version_clean(cleaned, numvers )


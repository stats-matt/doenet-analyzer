library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)
library(dplyr)

raw = stream_in(file(
  paste0(
    "https://www.doenet.org/api/getEventData.php?doenetId[]=_pdiqrEQqDLsTCucSaMdw1"
  )
))
events <-  raw$events[[1]]
# _PY82WGbGMv9FIVDzJdxgZ - this is the MN weird calc class
# _pdiqrEQqDLsTCucSaMdw1 - duane's survey
#_dKAX4QFX3JGXILGwaApZY
#_xmSpj9tMI84bWWWlp8UTm

dates <- pull_dates(events)
dates

min_date <- min(dates)
max_date <- max(dates)

cleaned <- clean_events(events, min(dates), max(dates))
summary_data <- summarize_events(cleaned)

summary_data %>%
  group_by(userId) %>%
  filter(pageCreditAchieved != "-Inf") %>%
  summarize(total = max(pageCreditAchieved, na.rm = TRUE)) %>%
  #mutate(total = max(pageCreditAchieved, na.rm = TRUE)) %>%
  ggplot(aes(x = total)) +
  geom_histogram() +
  labs(x = "Total Points", y = "Number of Students", title = "Total Scores on Assignment")


dates = unpacked %>% select(timestamp)

proc = anytime(dates$timestamp)
class(proc)
proc[1]
min(proc)
max(proc)
max(proc) > min(proc)


cleaned = clean_events(unpacked, min(proc), max(proc))
View(cleaned)
summarized = summarize_events(cleaned)
View(summarized)
summarized = summarized %>% group_by(response)
View(summarized)
interm = summarized %>% group_by(problem) %>% filter(creditAchieved < 1) %>% ggplot(aes(
  x = response,
  y = n,
  fill = as.factor(response)
)) + geom_col() + facet_wrap( ~ problem)
plot(interm)


source("functions.R")
numvers = pull_versions(cleaned)
numvers

cleaned_versions <- version_clean(cleaned, numvers)

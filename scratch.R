library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)
library(dplyr)

raw <-  stream_in(file(
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

min_date <- min(dates)
max_date <- max(dates)

cleaned_version <- clean_events(events, min(dates), max(dates))
summary_data_version <- summarize_events(cleaned)

cleaned <- version_filter(cleaned, 1)
summary_data <- summarize_events(cleaned)



cleaned_version %>% 
  filter(verb=="submitted" | verb == "answered") %>% 
  select(userId, response, responseText, item, componentName) %>% 
  filter(componentName == "/aboutSelf") %>% 
  View()


  ggplot(aes(x = as.character(responseText)))+
  geom_bar() +
  facet_wrap(~componentName, scales = "free")+
  labs(x = "Response", y = "Frequency")+
  coord_flip()



results <-
  events %>%
  select(userId, result)

results %>%
  mutate(new = map(
    result,
    ~ fromJSON(.)
  )) %>% 
  unnest_wider(new) %>% 
  View()


results


df <- tibble(
  x = 1:3,
  y = list(
    NULL,
    tibble(a = 1, b = 2),
    tibble(a = 1:3, b = 3:1)
  )
)
df
df %>% unnest(y)



########################################


summary_data %>%
  select(userId, item, response) %>% 
  distinct() %>% 
  filter(response!="list()") %>% 
  ggplot(aes(x = as.character(response)))+geom_bar()
  filter(!is.na(response)) %>%
  group_by(item) %>%
  ggplot(aes(
    x = as.factor(response),
    y = n,
    fill = as.factor(response)
  )) +
  geom_col() +
  facet_wrap( ~ item, scales = "free") +
  labs(x = "Answer", y = "Frequency", fill = "Answer")










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

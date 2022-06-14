library(tidyverse)
library(jsonlite)
library(anytime)
library(shiny)
library(tidytext)


x = stream_in(
  file(
    "https://www.doenet.org/api/getEventData.php?doenetId[]=_dKAX4QFX3JGXILGwaApZY"))
class(x)
head(x)
dim(x)
colnames(x)
rownames(x)

events <- x$events[[1]]
View(events)
cleaned = clean_events(events)
View(cleaned)




inter1 = cleaned %>%  select(userId, starts_with("X"), time, timestamp, pageNumber)
View(inter1)
inter2 = inter1 %>% group_by(userId, pageNumber)
View(inter2)
inter3 = inter2 %>% summarize_all("max", na.rm = T)
View(inter3)
inter4 = inter3 %>%  pivot_longer(cols = starts_with("X"),
                                  names_to = "problem",
                                  values_to = "score") 
View(inter4)
inter5 = inter4 %>% ungroup()
View(inter5)
inter6 = inter5 %>% filter(score != -Inf)
View(inter6)

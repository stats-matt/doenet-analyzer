library(tidyverse)
library(jsonlite)
library(anytime)
library(shiny)
library(tidytext)




raw_data = stream_in(
  file(
    "https://www.doenet.org/api/getEventData.php?doenetId[]=_dKAX4QFX3JGXILGwaApZY"))

data = raw_data$events[[1]]

cleaned = clean_events(data)
processed = data %>% group_by(activityCid) %>% summarize(min_stamp = min(timestamp))
processed[order(processed$min_stamp),]

cleaned$version_number = NA


dict = c(nrow(processed):1)
names(dict) = processed$activityCid
dict["bafkreidsigt2hh7mptrljpy6kdjew5bjlx6ccvfewjfzhavbcvucynyk6u"]

processed


for(i in (1:(nrow(cleaned)))){
  working_id = cleaned[[i,4]]
  cleaned[[i,25]] = dict[working_id]
}
View(cleaned)


check = cleaned %>% group_by(userId,version_num) %>% 
  mutate(total = sum(itemCreditAchieved,na.rm = TRUE)) %>% 
  mutate(avg = (total/(nrow(na.omit(itemCreditAchieved)))))

View(check)

scores = cleaned%>% group_by(userId,version_num) %>% select(starts_with("X")) %>% mutate(total = sum(na.rm = TRUE))
view(scores)  

datatest = c(1,2,3,4,5,6,7,8,9)
mat = matrix(datatest,nrow = 3)
sum(mat)
nrow(mat)
View(cleaned)


source("functions.R")
sumtwo = summarize_events(cleaned)
View(sumtwo)

intermed = sumtwo %>% group_by(problem,version_num) %>% mutate(avg = mean(score))
View(intermed)

View(summar)

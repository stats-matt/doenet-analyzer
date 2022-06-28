clean_events <- function(events,min_date,max_date) {
  # summarize events page
  events <-
    events %>%
    group_by(userId) %>%
    mutate(timestamp = anytime(timestamp)) %>%
    mutate(time = timestamp - min(timestamp))%>%
    ungroup()
  

  
  

events<- events %>% filter(between(timestamp, min_date,max_date))


events <-
  events %>%
  mutate(new = map(context, ~ fromJSON(.) %>% as.data.frame())) %>%
  unnest(new)

events <-
  events %>%
  mutate(new = map(object, ~ fromJSON(.) %>% as.data.frame())) %>%
  unnest(new)
events <- 
  events %>% 
  mutate(new = map(result, ~ fromJSON(.)%>% as.data.frame())) %>%
  unnest(new)
# 
# events <-
#   events %>%
#   separate(componentName, into = c(NA, "section", "answer", "type"))
# 
# events <-
#   events %>%
#   filter(!is.na(documentCreditAchieved))




events$version_num = NA
processed = events %>% group_by(activityCid) %>% summarize(min_stamp = min(timestamp))
processed = processed[order(processed$min_stamp),]
dict = c(1:nrow(processed))
names(dict) = processed$activityCid
for(i in (1:(nrow(events)))){
  working_id = events[[i,4]]
  events[[i,ncol(events)]] = dict[working_id]
}


return(events)

}

summarize_events <- function(data) {
  out <-
    data %>%
    select(userId, starts_with("X"), time, timestamp, pageNumber, version_num,response,creditAchieved) %>%
    group_by(userId, pageNumber, version_num) %>%
    pivot_longer(cols = starts_with("X"),
                 names_to = "problem",
                 values_to = "score") %>%
    ungroup() %>%
    filter(score != -Inf) %>% 
    group_by(problem,version_num) %>% 
    mutate(avg = mean(score))%>% 
    ungroup() %>%
    group_by(response) %>% 
    add_count(response) %>%
    ungroup()
  
  return(out)
}

pull_dates <- function(events){
  out <- events%>%select(timestamp)
  out <- anytime(out$timestamp)
  return(out)
  
}

pull_versions <- function(events){
  out <- events %>% distinct(activityCid)%>%nrow()
  return(out)
  
}

version_filter <- function(cleaned, input_version){
  out <- cleaned %>% filter(cleaned$version_num == input_version)
  return(out)
}
#Functions
#These are functions used by the server for the Doenet stats analyzer

#======================clean_events=============================================
#This function does most of the heavy lifting of cleaning the events data
#That consists of getting rid of events outside of date range, determining which
#event came from which version of the activity, and unpacking columns containing 
#JSON strings.

clean_events <- function(events,min_date,max_date) {
  
#This block adds the timestamp column to the cleaned data set
events <-
  events %>%
  group_by(userId) %>%
  mutate(timestamp = anytime(timestamp)) %>%
  mutate(time = timestamp - min(timestamp))%>%
  ungroup()


events<- events %>% filter(between(timestamp, min_date,max_date))


events <-
  events %>%
  mutate(new = map(object, ~ fromJSON(.) %>% as.data.frame())) %>%
  unnest(new)
events <-
  events %>%
  mutate(new = map(result, ~ fromJSON(.)%>% as.data.frame() %>% mutate_if(is.numeric, as.character))) %>%
  unnest(new)

events <-
  events %>%
  mutate(new = map(context, ~ fromJSON(., flatten = TRUE)%>% as.data.frame())) %>%
  unnest(new)

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

#=================summarize_events==============================================
#This creates the summary data
#We start with the cleaned data and select the relevant columns
#Then we group by userID and pageNumber and version_num
#Then the summarize_all gives us the max value (so the max time it took to
# answer that problem on that page, or something like that)
# pivot_longer sets up a column with all the problem names (drawn from the
# X1 - Xn columns in the cleaned set) and then takes the values from those
# Columns and drops them into the score column of the summary
summarize_events <- function(data) {
  out <-
    data %>%
    select(userId, item,itemCreditAchieved,pageCreditAchieved, time, timestamp, pageNumber, 
           version_num,response,creditAchieved) %>%
    group_by(userId, pageNumber, version_num) %>%
    filter(!(is.na(itemCreditAchieved))) %>% 
    group_by(item,version_num) %>% 
    mutate(avg = mean(itemCreditAchieved))%>% 
    ungroup() %>%
    group_by(response) %>% 
    add_count(response) %>%
    ungroup()
  
  return(out)
}
#====================pull_dates=================================================

# This function pulls the timestamp column out of the data set before cleaning
#   so that the clean function can only clean the data from the requested dates

pull_dates <- function(events){
  out <- events%>%select(timestamp)
  out <- anytime(out$timestamp)
  return(out)
}
#==================pull_versions================================================

# This function determines how many versions are present in the data set which is 
# done by checking the number of unique activityCIDs in the set. There could be
# a problem with this approach in that it does a poor job of accounting for 
# mutiple activities. Is this something that we need to address?

pull_versions <- function(events){
  out <- events %>% distinct(activityCid)%>%nrow()
  return(out)
}
#================version_filter=================================================



version_filter <- function(cleaned, input_version){
  out <- cleaned %>% filter(cleaned$version_num == input_version)
  return(out)
}
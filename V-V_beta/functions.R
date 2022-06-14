clean_events <- function(events) {
  # summarize events page
  
  #So this block is creating a column for timestamp (formatted nicely)
  #And then another column called time that is time since the first time 
  #the activity was used.
  events <-
    events %>%
    group_by(userId) %>%
    mutate(timestamp = anytime(timestamp)) %>%
    mutate(time = timestamp - min(timestamp))
  
  #This unpacks the json column context into a series of columns, one for each
  # question, as well as an answer ancestor, and credit achieved on each question
  # default value for no credit achieved is NA
  events <-
    events %>%
    mutate(new = map(context, ~ fromJSON(.) %>% as.data.frame())) %>%
    unnest(new)
  
  #This unpacks the json column object into name and componentName and 
  # componentType columns
  events <-
    events %>%
    mutate(new = map(object, ~ fromJSON(.) %>% as.data.frame())) %>%
    unnest(new)

  #So it is not entirely obvious what this next block does
  # The whole point of this is to create a nice column with version numbers, and 
  # it does that by using activityCid as a proxy. Essentially what this does, in
  # short, is group by activityCid, sort by minimum timestamp, and assign version
  # numbers accordingly. As usual its a little more complicated in practice
  
  #This just creates our empty version number column which we will eventually fill
  events$version_num = NA
  
  #This creates an N by 2 tibble where N is the number of unique activity Cids
  # First we group by activityCid, and then we summarize by minimum timestamp
  # First column of processed is activityCids, second column is minimum timestamp
  # for that Cid
  processed = events %>% group_by(activityCid) %>% summarize(min_stamp = min(timestamp))
  #This just orders processed so the first one is the 
  processed = processed[order(processed$min_stamp),]
  dict = c(1:nrow(processed))
  names(dict) = processed$activityCid
  
  for(i in (1:(nrow(events)))){
    working_id = events[[i,4]]
    events[[i,24]] = dict[working_id]
  }
  
  return(events)
}


#I do not think this is being used in the current deployed version 
# but I am using it so I am going to comment it anyway
# This gives us a data frame with a row for each question answered by each person
# so like unique(userID) * (number of questions) 


summarize_events <- function(data) {
  out <-
    data %>%
    select(userId, starts_with("X"), time, timestamp, pageNumber, version_num) %>%
    group_by(userId, pageNumber, version_num) %>%
    pivot_longer(cols = starts_with("X"),
                 names_to = "problem",
                 values_to = "score") %>%
    ungroup() %>%
    filter(score != -Inf) %>% 
    group_by(problem,version_num) %>% 
    mutate(avg = mean(score))%>% 
    ungroup()
  return(out)
}
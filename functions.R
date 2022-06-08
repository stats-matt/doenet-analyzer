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
# 
# events <-
#   events %>%
#   separate(componentName, into = c(NA, "section", "answer", "type"))
# 
# events <-
#   events %>%
#   filter(!is.na(documentCreditAchieved))
return(events)
}
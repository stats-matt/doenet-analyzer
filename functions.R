load_data <- function(query) {
  tmp_events <- data.frame()
  for (i in 1:length(unlist(query))) {
    raw <-  stream_in(file(
      paste0(
        "https://www.doenet.org/api/getEventData.php?doenetId[]=",
        query[[i]]
      )
    ))
    new_events <-  raw$events[[1]]
    tmp_events <- bind_rows(tmp_events, new_events)
  }
  return(tmp_events)
}


#======================clean_events=============================================
#This function does most of the heavy lifting of cleaning the events data
#That consists of getting rid of events outside of date range, determining which
#event came from which version of the activity, and unpacking columns containing
#JSON strings.

clean_events <- function(events, min_date, max_date) {
  events <- 
    events %>%
    filter(verb != "isVisible") %>%  # remove these since not used
    group_by(userId, doenetId) %>%
    mutate(timestamp = anytime(timestamp)) %>% #adds timestamp
    mutate(time_person = timestamp - min(timestamp)) %>% # time since person started activity
    ungroup() %>%
    mutate(time_activity = timestamp - min(timestamp)) %>% # time since activie was first loaded 
    #filter((timestamp > min_date) & (timestamp < max_date)) %>% # something's wrong with date/time filter
    mutate(new = map(context, ~ fromJSON(.) %>% as.data.frame())) %>% # separates context
    unnest_wider(new) %>%
    mutate(new = map(object, ~ fromJSON(.) %>% as.data.frame())) %>% # separates object
    unnest_wider(new) %>%
    mutate(response = map(result, ~ fromJSON(.))) %>% # separates results
    unnest_wider(response)
  events$version_num <-
    events$activityCid %>% as.factor() %>% as.numeric()
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
    select(
      userId,
      timestamp,
      time_person,
      time_activity,
      pageNumber,
      version_num,
      response,
      verb,
      item,
      itemCreditAchieved,
      pageCreditAchieved,
      pageVariantIndex,
      activityVariantIndex
    ) %>%
    filter(verb == "submitted") %>%
    group_by(item,
             pageNumber,
             version_num,
             pageVariantIndex,
             activityVariantIndex) %>%
    mutate(avg = mean(itemCreditAchieved)) %>%
    ungroup() %>%
    add_count(response,
              item,
              version_num,
              pageVariantIndex,
              activityVariantIndex)
  
  return(out)
}

#================version_filter=================================================
version_filter <- function(cleaned, input_version) {
  out <- cleaned #%>% filter(cleaned$version_num == input_version)
  return(out)
}

#==================pull_versions================================================
pull_versions <- function(events) {
  out <- events %>% distinct(activityCid) %>% nrow()
  return(out)
}

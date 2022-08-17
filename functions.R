#Functions
#These are functions used by the server for the Doenet stats analyzer

#======================clean_events=============================================
#This function does most of the heavy lifting of cleaning the events data
#That consists of getting rid of events outside of date range, determining which
#event came from which version of the activity, and unpacking columns containing
#JSON strings.

clean_events <- function(events, min_date, max_date) {
  #This block adds the timestamp column to the cleaned data set
  events <-
    events %>%
    group_by(userId) %>%
    mutate(timestamp = anytime(timestamp)) %>%
    mutate(time = timestamp - min(timestamp)) %>%
    ungroup()
  #events <-
  #  events %>% filter(between(timestamp, min_date, max_date))
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
    mutate(new = map(
      result,
      ~ fromJSON(.) %>% as.data.frame() %>% mutate_if(is.numeric, as.character)
    )) %>%
    unnest(new)
  
  #To solve the problem of multiple types in the response column we used code from
  # the following stackoverflow link:
  # https://stackoverflow.com/questions/27668266/dplyr-change-many-data-types
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
  for (i in (1:(nrow(events)))) {
    working_id = events[[i, 4]]
    events[[i, ncol(events)]] = dict[working_id]
  }
  return(events)
}


clean_events_no_dates <- function(events) {
  #This block adds the timestamp column to the cleaned data set
  events <-
    events %>%
    group_by(userId) %>%
    mutate(timestamp = anytime(timestamp)) %>%
    mutate(time = timestamp - min(timestamp)) %>%
    ungroup()
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
    mutate(new = map(
      result,
      ~ fromJSON(.) %>% as.data.frame() %>% mutate_if(is.numeric, as.character)
    )) %>%
    unnest(new)
  
  #To solve the problem of multiple types in the response column we used code from
  # the following stackoverflow link:
  # https://stackoverflow.com/questions/27668266/dplyr-change-many-data-types
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
  for (i in (1:(nrow(events)))) {
    working_id = events[[i, 4]]
    events[[i, ncol(events)]] = dict[working_id]
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
    # select(userId, starts_with("X"), time, timestamp, pageNumber,
    #        version_num,response,creditAchieved) %>%
    select(
      userId,
      item,
      time,
      timestamp,
      pageNumber,
      version_num,
      response,
      itemCreditAchieved,
      pageCreditAchieved
    ) %>%
    # group_by(userId, pageNumber, version_num) %>%
    # pivot_longer(cols = starts_with("X"),
    #              names_to = "problem",
    #              values_to = "score") %>%
    # ungroup() %>%
    #filter(score != -Inf) %>%
    group_by(item, version_num) %>%
    mutate(avg = mean(itemCreditAchieved)) %>%
    ungroup() %>%
    group_by(response) %>%
    add_count(response) %>%
    ungroup()
  
  return(out)
}
#====================pull_dates=================================================
pull_dates <- function(events) {
  out <- events %>% select(timestamp)
  out <- anytime(out$timestamp)
  return(out)
}
#==================pull_versions================================================
pull_versions <- function(events) {
  out <- events %>% distinct(activityCid) %>% nrow()
  return(out)
}
#================version_filter=================================================
version_filter <- function(cleaned, input_version) {
  out <- cleaned %>% filter(cleaned$version_num == input_version)
  return(out)
}

#Functions
#These are functions used by the server for the Doenet stats analyzer



# old code, maybe can be deleted
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

# this has been replaced by the line below it
# events$version_num <-  NA
# processed <-
#   events %>% group_by(activityCid) %>% summarize(min_stamp = min(timestamp))
# processed <-  processed[order(processed$min_stamp),]
# dict <- c(1:nrow(processed))
# names(dict) = processed$activityCid
# for (i in (1:(nrow(events)))) {
#   working_id = events[[i, 4]]
#   events[[i, ncol(events)]] = dict[working_id]
# }




#====================pull_dates=================================================
pull_dates <- function(events) {
  out <- events %>% select(timestamp)
  out <- anytime(out$timestamp)
  return(out)
}


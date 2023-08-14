# this is a scratch file to test new functions etc. locally before moving them to the shiny app

# load libraries
library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)
library(DT)
library(promises)
library(future)
library(memoise)

source("functions.R")

# load data (put in a doenetid - good doenetids to use are on slack)
#doenetid <- "_TETkqoYS3slQaDwjqkMrX"
#doenetid <- "_PY82WGbGMv9FIVDzJdxgZ"
doenetid <- "_ToZ1Ot0vL3eRyuai4Bkkd"
raw <-  stream_in(file(
  paste0(
    "https://www.doenet.org/api/getEventData.php?doenetId[]=",
    doenetid
  )
))

# clean the data, test the functions
events <-  raw$events[[1]]
dates <- pull_dates(events)
versions <- pull_versions(events)
min_date <- min(dates)
max_date <- max(dates)
cleaned_versions <- clean_events(events, min(dates), max(dates))
summary_data_versions <- summarize_events(cleaned_versions)
cleaned <- version_filter(cleaned_versions, 1)
summary_data <- summarize_events(cleaned)

###########################################
##### do local work below here
###########################################


# the implementations below are different ways of extracting the ids
# where code1 and code2 are not fully functional with code2 having a possible
# in infinite loop. Code3 works but it needs more urls for testing purposes
# skip to line 84
library(stringr)
library(rstudioapi)

extract_ids_code1 <- function(url) {
  start_index <- str_locate(url, "data=")[, 2]
  ids <- list()
  match_length <- attr(regexpr("data=", url), "match.length")
  
  while (start_index > -1) {
    end_index <- str_locate(url, "&data=", start = start_index)[, 2]
    if (end_index == -1) {
      ids <- c(ids, list(str_sub(url, start_index)))
      break
    }
    ids <- c(ids, list(str_sub(url, start_index, end_index - 1)))
    start_index <- end_index + match_length
  }
  
  return(ids)
}
extract_ids_code2 <- function(url) {
  start_index <- regexpr("data=", url)[1] + 5
  ids <- list()
  match_length <- attr(regexpr("data=", url), "match.length")
  
  
  while (start_index > 0) {
    end_index <- regexpr("&data=", url, start_index)[1]
    if (end_index == -1) {
      ids <- c(ids, list(substr(url,
                                start_index)))
      break
    }
    ids <- c(ids, list(substr(url, start_index, end_index - 1)))
    start_index <- end_index + match_length
  }
  
  return(ids)
}

extract_ids_code4 <- function(url) {
  url_1 <- gsub("&code=.*",
                "",
                as.character(url))
  url_2 <- sub('.*\\?', '', url_1)
  url_3 <- sub("data=",
               "&data=",
               url_2)
  
  ids <- strsplit(url_3,
                  "&data=")[[1]][-1]
  
  return(ids)
}
hashmap_id_failedattempt <- function(ids) {
  hashmap <- data.frame(matrix(
    nrow = 1,
    ncol = length(ids),
    dimnames = list(NULL,
                    paste0("Doenet id",
                           seq_along(ids)))
  ))
  hashmap[1,] <- ids
  
  return(hashmap)
}



# the follwoing functions are a work in progress the functions above are scrap
# work

url <-
  "https://doenet.shinyapps.io/analyzer/?data=AAA&data=BBB&data=CCC&data=DDD"

# the following code generates a list containing the doent ids as type of string
# good attempt but list is not a friendly type with dplyr package
extract_ids_code3 <- function(url) {
  url_1 <- gsub("&code=.*", "", as.character(url))
  url_2 <-
    sub("https://doenet.shinyapps.io/analyzer/\\?", "", url_1)
  url_3 <- sub("data=", "&data=", url_2)
  
  ids <- as.list(strsplit(url_3, "&data=")[[1]][-1])
  
  return(ids)
}

extract_ids_code3(url)

# the following function generates a hashmap with the appropriate amount
# of keys for the number of ids good attempt but list is not a friendly type
# with dplyer package
hashmap_ids <- function(ids) {
  hashmap <- list()
  for (index in seq_along(ids)) {
    key <- paste0("key", index)
    hashmap[[key]] <- ids[[index]]
  }
}



# Fetch the IDs from the extract_ids_code3 function
ids <- extract_ids_code3(url)

# Create the hashmap
hashmap <- hashmap_ids(ids)

# The following code chunk dynamically creates a dropdown depending on the length
# from the output of extract_ids_code3. The object df accepts a selected course
# id and process the information relative to that course id

ui <- fluidPage(
  selectInput("dropdown",
              "Select an option:",
              choices = NULL),
  tableOutput("data_table")
)

server <- function(input, output, session) {
  my_list <- names(hashmap)
  
  observe({
    updateSelectInput(session,
                      "dropdown",
                      choices = my_list)
  })
}

# Reactively update df based on the selected ID

df <- reactive({
  withProgress(message = 'Loading Data', {
    selected_key <- input$dropdown
    
    # Fetch the corresponding data from the hashmap
    selected_id <- hashmap[[selected_key]]
    
    # Use selected_id in the URL for fetching data
    url <- paste0(
      "https://www.doenet.org/api/getEventData.php?doenetId[]=",
      selected_id,
      "&code=",
      getQueryString()[["code"]]
    )
    
    data <- stream_in(file(url))
    return(data)
  })
})


#===============other implementation===========================================
# the following function generates a data frame with the appropriate amount
# of columns for the number of IDs

# the following code generates a character vector containing the document IDs
extract_ids_code3 <- function(url) {
  url_1 <- gsub("&code=.*", "", as.character(url))
  url_2 <-
    sub("https://doenet.shinyapps.io/analyzer/\\?", "", url_1)
  url_3 <- sub("data=", "&data=", url_2)
  
  ids <- strsplit(url_3, "&data=")[[1]][-1]
  
  return(ids)
}

hashmap_ids <- function(ids) {
  hashmap <- data.frame(matrix(
    nrow = 1,
    ncol = length(ids),
    dimnames = list(NULL, paste0("key", seq_along(ids)))
  ))
  hashmap[1,] <- ids
  
  return(hashmap)
}

# Convert the list to a data frame
ids_df <- data.frame(ids = extract_ids_code3(url))

# Create the hashmap as a data frame
hashmap <- hashmap_ids(ids_df$ids)

# Apply dplyr functions on the data frame
filtered_data <- ids_df %>%
  select(ids) %>%
  distinct()

# Reactively update df based on the selected ID

df <- reactive({
  withProgress(message = 'Loading Data', {
    selected_key <- input$dropdown
    
    # Fetch the corresponding data from the hashmap
    selected_id <- hashmap$ids[hashmap$key == selected_key]
    
    # Use selected_id in the URL for fetching data
    url <- paste0(
      "https://www.doenet.org/api/getEventData.php?doenetId[]=",
      selected_id,
      "&code=",
      getQueryString()[["code"]]
    )
    
    data <- stream_in(file(url))
    return(data)
  })
})




# install.packages("devtools")
devtools::install_github("r-lib/conflicted")
conflicted::conflicts_prefer(dplyr::select())


#====================Testing Shiny Output=======================================
library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)
library(dplyr)
library(DT)

extract_ids_code3 <- function(queryText) {
  url_1 <- gsub("&code=.*", "", queryText)
  url_2 <-
    sub("https://doenet.shinyapps.io/analyzer/\\?", "", url_1)
  url_3 <- sub("data=", "&data=", url_2)
  
  ids <- strsplit(url_3, "&data=")[[1]][-1]
  
  return(ids)
}

hashmap_ids <- function(ids) {
  hashmap <- data.frame(matrix(
    nrow = 1,
    ncol = length(ids),
    dimnames = list(NULL, paste0("key", seq_along(ids)))
  ))
  hashmap[1,] <- ids
  
  return(hashmap)
}

shinyApp(
  ui = fluidPage(verbatimTextOutput("query")),
  
  server = function(input, output, session) {
    output$query <- renderText({
      query <- getQueryString()
      queryText <- paste(names(query), query,
                         sep = "=", collapse = ", ")
      paste(
        "Your query string is:\n",
        queryText,
        "\n",
        "The hashmap contains the id:\n",
        paste(extract_ids_code3(queryText), collapse = ", ")
      )
    })
  }
)

#================Final server integration step==================================
library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)
library(dplyr)
library(tidyr)
library(scales)
library(DT)
library(stringr)
library(rstudioapi)

extract_ids_code3 <- function(queryText) {
  url_1 <- gsub("&code=.*", "", queryText)
  url_2 <-
    sub("https://doenet.shinyapps.io/analyzer/\\?", "", url_1)
  url_3 <- sub("data=", "&data=", url_2)
  
  ids <- strsplit(url_3, "&data=")[[1]][-1]
  
  return(ids)
}

hashmap_ids <- function(ids) {
  if (length(ids) > 0) {
    course_ids <- paste("Course ID", seq_along(ids))
    hashmap <-
      data.frame(course_id = ids, course_id_display = course_ids)
  } else {
    hashmap <-
      data.frame(
        course_id = character(),
        course_id_display = character(),
        stringsAsFactors = FALSE
      )
  }
  
  return(hashmap)
}

shinyApp(
  ui = fluidPage(
    verbatimTextOutput("query"),
    selectInput("dropdown", "Select an option:", choices = NULL)
  ),
  
  server = function(input, output, session) {
    output$query <- renderText({
      query <- getQueryString()
      queryText <- paste(names(query), query,
                         sep = "=", collapse = ", ")
      ids <- extract_ids_code3(queryText)
      hashmap <- hashmap_ids(ids)
      
      paste(
        "Your query string is:\n",
        queryText,
        "\n",
        "The hashmap contains the id:\n",
        paste(ids, collapse = ", ")
      )
    })
    
    observe({
      queryText <- paste(names(getQueryString()),
                         getQueryString(),
                         sep = "=",
                         collapse = ", ")
      ids <- extract_ids_code3(queryText)
      hashmap <- hashmap_ids(ids)
      
      updateSelectInput(session, "dropdown", choices = hashmap$course_id_display)
    })
    
    df <- reactive({
      withProgress(message = 'Loading Data', {
        selected_display <- input$dropdown
        
        hashmap <- hashmap_ids(extract_ids_code3(getQueryString()))
        selected_id <-
          hashmap$course_id[hashmap$course_id_display == selected_display]
        
        if (length(selected_id) > 0) {
          url <- paste0(
            "https://www.doenet.org/api/getEventData.php?doenetId[]=",
            selected_id,
            "&code=",
            getQueryString()[["code"]]
          )
          
          data <- stream_in(file(url))
          return(data)
        } else {
          return(NULL)
        }
      })
    })
  }
)

#===============JSON implementation hashmap down here===========================

getQueryText <- reactive({
  query <- getQueryString()
  queryText <-
    paste(names(query), query, sep = "=", collapse = ", ")
  return(queryText)
})

extract_ids_code3 <- function(queryText) {
  url_1 <- gsub("&code=.*", "", queryText)
  url_2 <-
    sub("https://doenet.shinyapps.io/analyzer/\\?", "", url_1)
  url_3 <- sub("data=", "&data=", url_2)
  
  ids <- strsplit(url_3, "&data=")[[1]][-1]
  
  return(ids)
}

hashmap_ids <- function(ids) {
  if (length(ids) > 0) {
    course_ids <- paste("Course ID", seq_along(ids))
    hashmap <-
      data.frame(
        course_id = ids,
        course_id_display = course_ids,
        stringsAsFactors = FALSE
      )
  } else {
    hashmap <-
      data.frame(
        course_id = character(),
        course_id_display = character(),
        stringsAsFactors = FALSE
      )
  }
  
  return(hashmap)
}

observe({
  queryText <- isolate(getQueryText())
  ids <- extract_ids_code3(queryText)
  hashmap <- hashmap_ids(ids)
  
  updateSelectInput(session, "dropdown", choices = hashmap$course_id_display)
})

df <- reactive({
  withProgress(message = 'Loading Data', {
    selected_display <- input$dropdown
    
    hashmap <- hashmap_ids(extract_ids_code3(getQueryText()))
    selected_id <-
      hashmap$course_id[hashmap$course_id_display == selected_display]
    
    if (is.null(selected_id)) {
      # Load default dataset when no option is selected
      default_url <- paste0(
        "https://www.doenet.org/api/getEventData.php?doenetId[]=",
        getQueryString()[["data"]],
        # this is the web version
        "&code=",
        getQueryString()[["code"]]
      )
      data <- stream_in(file(default_url))
    } else {
      # Load data based on selected option
      selected_url <- paste0(
        "https://www.doenet.org/api/getEventData.php?doenetId[]=",
        selected_id,
        "&code=",
        getQueryString()[["code"]]
      )
      data <- stream_in(file(selected_url))
    }
    
    return(data)
  })
})

#================code works but it loads the data in twice======================

getQueryText <- reactive({
  query <- getQueryString()
  queryText <-
    paste(names(query), query, sep = "=", collapse = ", ")
  return(queryText)
})
extract_ids_code3 <- function(queryText) {
  if (is.null(queryText)) {
    return(character())
  } else {
    url_1 <- gsub("&code=.*", "", queryText)
    url_2 <-
      sub("https://doenet.shinyapps.io/analyzer/\\?", "", url_1)
    url_3 <- sub("data=", "&data=", url_2)
    ids <- strsplit(url_3, "&data=")[[1]][-1]
    return(ids)
  }
}
hashmap_ids <- function(ids) {
  if (length(ids) > 0) {
    course_ids <- paste("Course ID", seq_along(ids))
    hashmap <-
      data.frame(
        course_id = ids,
        course_id_display = course_ids,
        stringsAsFactors = FALSE
      )
  } else {
    hashmap <-
      data.frame(
        course_id = character(),
        course_id_display = character(),
        stringsAsFactors = FALSE
      )
  }
  return(hashmap)
}
extract_values <- function(hashmap) {
  values_list <- data.frame(course_id = hashmap$course_id)
  return(values_list)
}
df_original_json <- function(hashmap) {
  values_list <- extract_values(hashmap)
  df_list <- list()
  
  for (value in values_list) {
    url <- paste0(
      "https://www.doenet.org/api/getEventData.php?doenetId[]=",
      value,
      "&code=",
      getQueryString()[["code"]]
    )
    
    data <- stream_in(file(url))
    
    # Check if 'data' is not NULL before proceeding
    if (!is.null(data)) {
      df_list[[value]] <- data
    }
  }
  
  return(df_list)
}
hashmap_df_json <- function(df_list, ids) {
  if (length(ids) > 0) {
    course_ids <- paste("Course ID", seq_along(ids))
    hashmap <-
      data.frame(json_data = df_list, selected_display = course_ids)
  } else {
    hashmap <- list()
  }
  
  return(hashmap)  # Corrected the return statement to return hashmap
}
#Observe block to update dropdown choices
observe({
  queryText <- isolate(getQueryText())
  ids <- extract_ids_code3(queryText)
  hashmap <- hashmap_ids(ids)
  
  updateSelectizeInput(session, "dropdown", choices = hashmap$course_id_display)
})
# Create a reactive value to store the loaded data
df <- reactive({
  withProgress(message = "Doenet analyzer is loading your data,
                 please be wait patiently.", {
                   selected_display <- input$dropdown
                   b <-
                     extract_values(hashmap_ids(extract_ids_code3(getQueryText())))
                   c <- df_original_json(b)
                   d <- extract_ids_code3(getQueryText())
                   hashmap <-
                     hashmap_df_json(c, d)  # Call df_list() as a reactive expression
                   selected_id <- hashmap[[selected_display]]
                   
                   if (is.null(selected_id)) {
                     # Load default dataset when no option is selected
                     default_url <- paste0(
                       "https://www.doenet.org/api/getEventData.php?doenetId[]=",
                       getQueryString()[["data"]],
                       # this is the web version
                       "&code=",
                       getQueryString()[["code"]]
                     )
                     data <- stream_in(file(default_url))
                   } else {
                     # Load data based on selected option
                     selected_url <- paste0(
                       "https://www.doenet.org/api/getEventData.php?doenetId[]=",
                       selected_id,
                       "&code=",
                       getQueryString()[["code"]]
                     )
                     data <- stream_in(file(selected_url))
                   }
                   
                   return(data)
                 })
})

# df_original <- reactive({
#   withProgress(message = 'Loading Data', {
#     stream_in(file(
#       paste0(
#         "https://www.doenet.org/api/getEventData.php?doenetId[]=",
#         getQueryString()[["data"]], # this is the web version
#         "&code=",
#         getQueryString()[["code"]]
#       )
#     ))})
# })


#============================TEMP CODE==========================================
getQueryText <- reactive({
  query <- getQueryString()
  queryText <-
    paste(names(query), query, sep = "=", collapse = ", ")
  return(queryText)
})
extract_ids_code3 <- function(queryText) {
  if (is.null(queryText)) {
    return(character())
  } else {
    url_1 <- gsub("&code=.*", "", queryText)
    url_2 <-
      sub("https://doenet.shinyapps.io/analyzer/\\?", "", url_1)
    url_3 <- sub("data=", "&data=", url_2)
    ids <- strsplit(url_3, "&data=")[[1]][-1]
    return(ids)
  }
}
extract_values <- function(hashmap) {
  values_list <- data.frame(course_id = hashmap$course_id)
  return(values_list)
}
df_original_json <- function(hashmap) {
  values_list <- extract_values(hashmap)
  df_list <- list()
  
  for (value in values_list) {
    url <- paste0(
      "https://www.doenet.org/api/getEventData.php?doenetId[]=",
      value,
      "&code=",
      getQueryString()[["code"]]
    )
    
    data <- stream_in(file(url))
    
    # Check if 'data' is not NULL before proceeding
    if (!is.null(data)) {
      df_list[[value]] <- data
    }
  }
  
  return(df_list)
}
hashmap_df_json <- function(df_list, ids) {
  if (length(ids) > 0) {
    course_ids <- paste("Course ID", seq_along(ids))
    hashmap <-
      data.frame(json_data = df_list, selected_display = course_ids)
  } else {
    hashmap <- list()
  }
  
  return(hashmap)  # Corrected the return statement to return hashmap
}
observe({
  queryText <- isolate(getQueryText())
  ids <- extract_ids_code3(queryText)
  hashmap <- hashmap_ids(ids)
  
  updateSelectizeInput(session, "dropdown", choices = hashmap$course_id_display)
})

# Wrap the df_original_json function with memoise
df_original_json_memo <- memoise(df_original_json)

# Create a reactive value to store the loaded data
df <- reactive({
  withProgress(message = "Doenet analyzer is loading your data, please be patient.", {
    selected_display <- input$dropdown
    b <-
      extract_values(hashmap_ids(extract_ids_code3(getQueryText())))
    
    # Use the memoized version of df_original_json directly
    hashmap <- df_original_json_memo(b)
    
    selected_id <-
      hashmap$json_data[[selected_display]]  # Retrieve the selected data from the hashmap
    
    if (is.null(selected_id)) {
      # Load default dataset when no option is selected
      default_url <- paste0(
        "https://www.doenet.org/api/getEventData.php?doenetId[]=",
        getQueryString()[["data"]],
        # this is the web version
        "&code=",
        getQueryString()[["code"]]
      )
      data <- stream_in(file(default_url))
    } else {
      # Use the selected data from the hashmap
      data <- selected_id
    }
    
    return(data)
  })
})


#==========================Testing out reordering by frequency==================
library(ggplot2)
library(forcats)

library(forcats)

cleaned_versions %>%
  filter(verb %in% c("submitted", "answered", "selected")) %>%
  select(itemCreditAchieved,
         userId,
         response,
         responseText,
         item,
         componentName,
         pageNumber) %>%
  filter(
    componentName != "/aboutSelf" &
      !is.na(pageNumber) & !is.na(item) & !is.na(responseText)
  ) %>%
  filter(responseText != "NULL" & responseText != "＿") %>%
  filter(itemCreditAchieved < 1) %>%
  group_by(pageNumber, item, responseText) %>%
  summarise(n = n()) %>%
  filter(n >= 10) %>%
  ungroup() %>%
  mutate(responseText = fct_reorder(as.character(responseText),
                                    n,
                                    .desc = TRUE) %>% fct_rev()) %>%
  ggplot(aes(x = responseText, y = n)) +
  geom_col() +
  facet_wrap( ~ pageNumber + item, scales = "free") +
  labs(x = "Wrong Answer", y = "Frequency (if more than 10 times)") +
  coord_flip()

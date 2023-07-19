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
  url_2 <- sub("https://doenet.shinyapps.io/analyzer/\\?", "", url_1)
  url_3 <- sub("data=", "&data=", url_2)
  
  ids <- strsplit(url_3, "&data=")[[1]][-1]
  
  return(ids)
}

hashmap_ids <- function(ids) {
  if (length(ids) > 0) {
    course_ids <- paste("Course ID", seq_along(ids))
    hashmap <- data.frame(course_id = ids, course_id_display = course_ids)
  } else {
    hashmap <- data.frame(course_id = character(), course_id_display = character(), stringsAsFactors = FALSE)
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
      
      paste("Your query string is:\n", queryText, 
            "\n",
            "The hashmap contains the id:\n",
            paste(ids, collapse = ", "))
    })
    
    observe({
      queryText <- paste(names(getQueryString()), getQueryString(),
                         sep = "=", collapse = ", ")
      ids <- extract_ids_code3(queryText)
      hashmap <- hashmap_ids(ids)
      
      updateSelectInput(session, "dropdown", choices = hashmap$course_id_display)
    })
    
    df <- reactive({
      withProgress(message = 'Loading Data', {
        stream_in(file(
          paste0(
            "https://www.doenet.org/api/getEventData.php?doenetId[]=",
            #doenetid, # this is for local work
            #"",
            getQueryString()[["data"]], # this is the web version
            "&code=",
            getQueryString()[["code"]]
          )
        ))})
    })
  }
)


df <- reactive({
  withProgress(message = 'Loading Data', {
    selected_display <- input$dropdown
    
    # Check if the dropdown is empty or not
    if (is.null(selected_display)) {
      # Load default dataset
      url <- paste0(
        "https://www.doenet.org/api/getEventData.php?doenetId[]=",
        default_doenetid,
        "&code=",
        getQueryString()[["code"]]
      )
    } else {
      # Load dataset based on selected ID
      hashmap <- hashmap_ids(extract_ids_code3(isolate(getQueryText())))
      selected_id <- hashmap$course_id[hashmap$course_id_display == selected_display]
      
      if (length(selected_id) > 0) {
        url <- paste0(
          "https://www.doenet.org/api/getEventData.php?doenetId[]=",
          selected_id,
          "&code=",
          getQueryString()[["code"]]
        )
      } else {
        # Handle case when selected ID is not found
        return(NULL)
      }
    }
    
    data <- NULL
    tryCatch({
      data <- jsonlite::fromJSON(url)
    }, error = function(e) {
      # Handle error
      # You can display an error message or take appropriate action
      message("Error loading data:", conditionMessage(e))
    })
    
    return(data)
  })
})

library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)
library(scales)
library(DT)
library(rstudioapi)
library(memoise)
library(ggradar) # devtools::install_github("ricardo-bion/ggradar")

shinyServer(function(input, output, session) {
  source("./functions.R")
  
  query <- reactive({
    getQueryString()
  })
  
  output$num_ids <- renderText(length(paste(query())))
  
  observe({
    updateSelectInput(
      session = session,
      inputId = "activity_select",
      choices = c("All", paste(query())),
    )
  })
  
  events <- reactive({
    load_data(query())
  })
  
  # events <- reactive({
  #   if (length(paste(query()))==1){events_full()}
  #   else if (input$activity_select == "All") {events_full()}
  # })
  
  # events <- reactive({
  #   if (input$activity_select == "All") {
  #     events_full()
  #   }
  #   else{
  #     events_full() %>% filter(doenetId == input$activity_select)
  #   }
  # })
  
  
  
  
  
  # ==========PRE CLEANING INPUTS==========================
  
  # ==========================GETTING DATA===================
  # What this code is doing is pulling in the data
  # getQueryString() is a function that takes a query string and turns it into
  # a list, which allows us to find the "data" item of that list.
  # By default it pulls the query string from the app environment (?)
  # renderText turns it that list into a string and then checks if it is null
  # This is a check to make sure we are in fact looking for data that exists
  
  # Stream_in unpacks the json file we get from the URL into a 1 by 3 dataframe
  # First element is a boolean that tells if it was successful or not
  # Second element is a message (typically empty right now)
  # Third is the list that contains the event log
  # df contains this 1 by 3 frame at the end of this block
  # Install and load the 'promises' package
  
  source("./setup.R", local = TRUE)
  
  #events <- reactive({events_full()}
  # if (input$activity_select == "All") {
  #   if (input$version_select == "All") {
  #     events_full()
  #   }
  #   else{
  #     events_full() %>% filter(version == input$version_select)
  #   }
  # }
  #   if (input$version_select == "All") {
  #     events_full() %>% filter(doenetId == input$activity_select)
  #   }
  #   else{
  #     events_full() %>% filter(version == input$version_select) %>% filter(doenetId == input$activity_select)
  #   }
  # }
  #)
  
  # this is the hash method
  #
  #
  # # Takes the string of url and splices it to return a list of strings
  #   getQueryText <- reactive({
  #   query <- getQueryString()
  #   queryText <- paste(names(query), query, sep = "=", collapse = ", ")
  #   return(queryText)
  # })
  #
  # # Takes in a list of strings and splices it to return a list of doenet id
  # extract_ids_code3 <- function(queryText) {
  #   if (is.null(queryText)) {
  #     return(character())
  #   } else {
  #     url_1 <- gsub("&code=.*", "", queryText)
  #     url_2 <- sub("https://doenet.shinyapps.io/analyzer/\\?", "", url_1)
  #     url_3 <- sub("data=", "&data=", url_2)
  #     ids <- strsplit(url_3, "&data=")[[1]][-1]
  #     return(ids)
  #   }
  # }
  #
  # # Stores a list of doenet ids and returns a data frame of doenet ids
  # hashmap_ids <- function(ids) {
  #   if (length(ids) > 0) {
  #     course_ids <- paste("Course ID", seq_along(ids))
  #     hashmap <- data.frame(course_id = ids, course_id_display = course_ids, stringsAsFactors = FALSE)
  #   } else {
  #     hashmap <- data.frame(course_id = character(), course_id_display = character(), stringsAsFactors = FALSE)
  #   }
  #   return(hashmap)
  # }
  #
  # # Takes in a data frame of doenet ids and returns a data frame of the doenet
  # # ids
  # extract_values <- function(hashmap) {
  #   values_list <- data.frame(course_id = hashmap$course_id)
  #   return(values_list)
  # }
  #
  # # Takes in a data frame of doenet ids and returns a list of api calls for the
  # # json file of the doenet id
  # df_original_json <- function(hashmap) {
  #   values_list <- extract_values(hashmap)
  #   df_list <- list()
  #
  #   for (value in values_list) {
  #     url <- paste0(
  #       "https://www.doenet.org/api/getEventData.php?doenetId[]=",
  #       value,
  #       "&code=",
  #       getQueryString()[["code"]]
  #     )
  #
  #     data <- stream_in(file(url))
  #
  #     # Check if 'data' is not NULL before proceeding
  #     if (!is.null(data)) {
  #       df_list[[value]] <- data
  #     }
  #   }
  #
  #   return(df_list)
  # }
  #
  # # Takes in a list of api calls and returns a data frame with the api call and
  # # the course number
  # hashmap_df_json <- function(df_list, ids) {
  #   if (length(ids) > 0) {
  #     course_ids <- paste("Course ID", seq_along(ids))
  #     hashmap <- data.frame(json_data = df_list, selected_display = course_ids)
  #   } else {
  #     hashmap <- list()
  #   }
  #
  #   return(hashmap)  # Corrected the return statement to return hashmap
  # }
  # #Observe block to update dropdown choices
  # observe({
  #   queryText <- isolate(getQueryText())
  #   ids <- extract_ids_code3(queryText)
  #   hashmap <- hashmap_ids(ids)
  #
  #   updateSelectizeInput(session, "dropdown", choices = hashmap$course_id_display)
  # })
  # # Create a reactive value to store the loaded data
  # df <- reactive({
  #   withProgress(message = "Doenet analyzer is loading your data,
  #                please be wait patiently.", {
  #                  selected_display <- input$dropdown
  #                  b <- extract_values(hashmap_ids(extract_ids_code3(getQueryText())))
  #                  c <- df_original_json(b)
  #                  d <- extract_ids_code3(getQueryText())
  #                  hashmap <- hashmap_df_json(c, d)  # Call df_list() as a reactive expression
  #                  selected_id <- hashmap[[selected_display]]
  #
  #                  if (is.null(selected_id)) {
  #                    # Load default dataset when no option is selected
  #                    default_url <- paste0(
  #                      "https://www.doenet.org/api/getEventData.php?doenetId[]=",
  #                      getQueryString()[["data"]], # this is the web version
  #                      "&code=",
  #                      getQueryString()[["code"]]
  #                    )
  #                    data <- stream_in(file(default_url))
  #                  } else {
  #                    # Load data based on selected option
  #                    selected_url <- paste0(
  #                      "https://www.doenet.org/api/getEventData.php?doenetId[]=",
  #                      selected_id,
  #                      "&code=",
  #                      getQueryString()[["code"]]
  #                    )
  #                    data <- stream_in(file(selected_url))
  #                  }
  #
  #                  return(data)
  #                })
  # })
  
  # ====================PROCESSING DATA=========
  # This block pulls out the events log, which is a dataframe, within a
  # 1 element list within a 1 by 3 dataframe. So df is the frame,
  # events is an named column of df, which contains one element, which is a
  # dataframe containing the events log, which we are then assigning to a local
  # variable called events. Note the difference between the events column of df
  # and our local events object (even though they are essentially the same data)
  
  # if there is no data in the url, it uses a default base dataset
  
  # this has been removed since we don't need to worry about this case, and it doesn't fix the error anyhow
  # events <- reactive({
  #   if('data' %in% names(getQueryString())){
  #     events <- {df()$events[[1]]}
  #   } else {
  #     events <- read.csv("base.csv")
  #   }})
  
  # Takes our events and cleans them up and adds some helpful columns
  # See file functions.R for more information.
  
  # A note on how data is currently structured:
  # There are four working sets:
  # cleaned_versions -> all the way cleaned except including data from all versions
  #                    of the activity (needed for version comparison)
  # summary_data_versions -> summary of the cleaned_version set by problem
  #                         needed to do version by version by problem comparisons
  # cleaned -> the true cleaned data, which is cleaned filtered to look at the
  #             selected version. This is used for all non-cross-version plots.
  #             For more on this filter system, please consult functions.R
  # summary_data -> summary data by problem from cleaned, only looking at one version.
  #                 Used to do problem by problem work when not looking across versions.
  
  source("./make_datasets.R", local = TRUE)
  
  # This outputs the version selection and the date slider for the UI
  # output$version_select <- renderUI({
  #   selectInput("version_selected", "Version: ", c(1:versions()))
  # })
  
  output$act <- renderText(input$activity_select)
  
  # =========================DOWNLOADING DATA======================================
  # This gives allows the user to download the data shown in a csv file for their
  # own purposes
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("events-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(events(), file)
    }
  )
  
  source("./text_and_tables.R", local = TRUE)
  
  source("./make_plots.R", local = TRUE)
  
})

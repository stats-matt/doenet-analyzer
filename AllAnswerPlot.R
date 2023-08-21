library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)
library(dplyr)
library(DT)
library(promises)
library(future)
library(ggplot2)
library(plotly)

# load data (put in a doenetid - good doenetids to use are on slack)
#doenetid <- "_TETkqoYS3slQaDwjqkMrX"
doenetid <- "_ds4TPv9RAI9b3DnYvgROo"
#doenetid <- "_dKAX4QFX3JGXILGwaApZY"
raw <-  stream_in(file(
  paste0(
    "https://www.doenet.org/api/getEventData.php?doenetId[]=",
    doenetid
  )
))

# clean the data, test the functions
events <-  raw$events[[1]]
#events <- read.csv("base.csv")
#events <- events[1:100,]
dates <- pull_dates(events)
versions <- pull_versions(events)
min_date <- min(dates)
max_date <- max(dates)
cleaned_versions <- clean_events(events, min(dates), max(dates))

ui <- fluidPage(
  fluidRow(
    column(6, selectInput("page_dropdown_all", "Select Page", "")),
    column(6, selectInput("item_dropdown_all", "Select Item", ""))
  ),
  sliderInput("integer_slider", "Most Frequent Answers",
              min = 0, 
              max = 0,
              value = 0),
  # Render the plot
  plotlyOutput("all_answers_plot")
  )


server <- function(input, output, session) {

  
  # items for dropdown menu dynamical generation
  facet_naming_item <- function() {
    testing_item <- cleaned_versions %>%
      filter(verb %in% c("submitted", "answered", "selected")) %>%
      select(item, pageNumber, componentName, responseText) %>%
      filter(componentName != "/aboutSelf" & !is.na(pageNumber) & !is.na(item) &
               responseText != "NULL" & responseText != "＿") %>%
      group_by(item, pageNumber) %>%
      count(responseText) %>%
      ungroup() %>%
      distinct(item)
    
    natural_naming_items <- paste0("Item ", testing_item$item)
    return(natural_naming_items)
  }
  # pages for dropdown menu dynamical generation
  facet_naming_page <- function() {
    testing_page <- cleaned_versions %>%
      filter(verb %in% c("submitted", "answered", "selected")) %>%
      select(item, pageNumber, componentName, responseText) %>%
      filter(componentName != "/aboutSelf" & !is.na(pageNumber) & !is.na(item) &
               responseText != "NULL" & responseText != "＿") %>%
      group_by(item, pageNumber) %>%
      count(responseText) %>%
      ungroup() %>%
      distinct(pageNumber)
    
    natural_naming_page <- paste0("Page ", testing_page$pageNumber)
    return(natural_naming_page)
  }
  max_value_sliderdf <- reactiveVal(0)  # Use a reactiveVal to store the max value
  # Observe changes in the item dropdown and update choices for page dropdown accordingly
  observe({
    updateSelectInput(session, "item_dropdown_all", choices = facet_naming_item())
    updateSelectInput(session, "page_dropdown_all", choices = facet_naming_page())
  })
  observe({
    item_selected_all <- input$item_dropdown_all
    page_selected_all <- input$page_dropdown_all
    
    a <- cleaned_versions %>%
      filter(verb %in% c("submitted", "answered", "selected")) %>%
      select(itemCreditAchieved, userId, response, responseText, item, componentName, pageNumber) %>%
      filter(componentName != "/aboutSelf" & !is.na(pageNumber) & !is.na(item) & !is.na(responseText)) %>% 
      filter(responseText != "NULL" & responseText != "＿") %>%
      filter(item == gsub("Item ", "", item_selected_all)) %>%
      filter(pageNumber == gsub("Page ", "", page_selected_all)) %>%
      group_by(pageNumber, item, responseText) %>%  
      summarise(n = n()) %>%  
      filter(n >= 10) %>%
      ungroup() %>% 
      mutate(responseText = fct_reorder(
        as.character(responseText),
        n,
        .desc = TRUE
      ) %>% fct_rev())
    
    if (nrow(a) > 0) {
      max_value_sliderdf(max(a$n))  # Update the reactive value
      # Update the sliderInput dynamically
      updateSliderInput(session, "integer_slider", max = max_value_sliderdf(), value = max_value_sliderdf())
    } else {
      max_value_sliderdf(0)  # Reset the reactive value to 0
      # If 'a' is empty, set the maximum and default value of the slider to 0 or any other appropriate default value
      updateSliderInput(session, "integer_slider", max = 0, value = 0)
    }
  })
  # Create a Plotly bar plot using the filtered data
  output$all_answers_plot <- renderPlotly({
    item_selected_all <- input$item_dropdown_all
    page_selected_all <- input$page_dropdown_all
    slider_selected_all <- input$integer_slider
    
    clean_data <- cleaned_versions  # Extract the value from the reactive expression
    
    filtered_data_all <- clean_data %>%
      filter(verb %in% c("submitted", "answered", "selected")) %>%
      select(item, pageNumber, componentName, responseText) %>%
      filter(!is.na(pageNumber)) %>% 
      filter(!is.na(item)) %>% 
      filter(responseText != "NULL") %>%
      filter(responseText != "＿") %>% 
      filter(item == gsub("Item ", "", item_selected_all)) %>%
      filter(pageNumber == gsub("Page ", "", page_selected_all)) %>%
      group_by(responseText) %>% 
      summarise(n = n()) %>%  
      filter(n >= 10) %>%
      filter(n %in% (0:slider_selected_all)) %>%
      ungroup() %>% 
      mutate(responseText = fct_reorder(
        as.character(responseText),
        n,
        .desc = TRUE
      ) %>% fct_rev())
    
    if (nrow(filtered_data_all) == 0) {
      # Create an empty plot if no data points are available
      return(plot_ly(x = NULL, y = NULL, type = "bar"))
    } else {
      # Create a Plotly bar plot
      return(plot_ly(data = filtered_data_all, 
                     x = ~n, 
                     y = ~responseText, 
                     type = "bar") %>%
               layout(
                 yaxis = list(title = "All Answer"),  # Swap x-axis and y-axis titles
                 xaxis = list(title = "Frequency (if more than 10 times)"),  # Swap x-axis and y-axis titles
                 title = paste("Graph of",
                               page_selected_all,
                               "With",
                               item_selected_all),
                 barmode = "stack"  # Add this line to adjust bar mode if needed
               ))
    }
  })
  
}

shinyApp(ui, server)
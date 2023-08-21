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
doenetid <- "_TETkqoYS3slQaDwjqkMrX"
#doenetid <- "_ds4TPv9RAI9b3DnYvgROo"
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
    column(6, selectInput("page_dropdown", "Select Page", "")),
    column(6, selectInput("item_dropdown", "Select Item", ""))
  ),
  sliderInput("slider_wrong", 
              "Most Frequent Wrong Answers",
              min = 0, 
              max = 0,
              value = 0),
  # Render the plot
  plotlyOutput("wrong_plot")
)


server <- function(input, output, session) {
  
  # items for dropdown menu dynamical generation
  facet_naming_item <- function() {
    testing_item <- cleaned_versions %>%
      filter(verb %in% c("submitted", "answered", "selected")) %>%
      select(item) %>%
      filter(!is.na(item)) %>%
      distinct(item)
    
    natural_naming_items <- paste0("Item ", testing_item$item)
    return(natural_naming_items)
  }
  # pages for dropdown menu dynamical generation
  facet_naming_page <- function() {
    testing_page <- cleaned_versions %>%
      filter(verb %in% c("submitted", "answered", "selected")) %>%
      select(pageNumber) %>%
      filter(!is.na(pageNumber)) %>%
      distinct(pageNumber)
    
    natural_naming_page <- paste0("Page ", testing_page$pageNumber)
    return(natural_naming_page)
  }
  
  # Observe changes in the item dropdown and update choices for page dropdown accordingly
  observe({
    updateSelectInput(session, "item_dropdown", choices = facet_naming_item())
    updateSelectInput(session, "page_dropdown", choices = facet_naming_page())
  })
  # Observe changes in the item and page dropdowns
  max_value_sliderdf <- reactiveVal(0)  # Use a reactiveVal to store the max value
  observe({
    item_selected <- input$item_dropdown
    page_selected <- input$page_dropdown
    
    cleaned_data <- cleaned_versions
    
    wrong <- cleaned_data %>%
      filter(verb %in% c("submitted", "answered", "selected")) %>%
      select(itemCreditAchieved, userId, response, responseText, item, componentName, pageNumber) %>%
      filter(componentName != "/aboutSelf" & !is.na(pageNumber) & !is.na(item) & !is.na(responseText)) %>% 
      filter(responseText != "NULL" & responseText != "＿") %>%
      filter(itemCreditAchieved < 1) %>%
      filter(item == gsub("Item ", "", item_selected)) %>%
      filter(pageNumber == gsub("Page ", "", page_selected)) %>%
      group_by(pageNumber, item, responseText) %>%  
      summarise(n = n()) %>%  
      filter(n >= 10) %>%
      ungroup() %>% 
      mutate(responseText = fct_reorder(
        as.character(responseText),
        n,
        .desc = TRUE
      ) %>% fct_rev())
    
    if (nrow(wrong) > 0) {
      max_value_sliderdf(max(wrong$n))  # Update the reactive value
      # Update the sliderInput dynamically
      updateSliderInput(session, "slider_wrong", max = max_value_sliderdf(), value = max_value_sliderdf())
    } else {
      max_value_sliderdf(0)  # Reset the reactive value to 0
      # If 'a' is empty, set the maximum and default value of the slider to 0 or any other appropriate default value
      updateSliderInput(session, "slider_wrong", max = 0, value = 0)
    }
  })
  # Inside the output$wrong_plot renderPlot function
  output$wrong_plot <- renderPlotly({
    item_selected <- input$item_dropdown
    page_selected <- input$page_dropdown
    slider_selected <- input$slider_wrong
    
    cleaned_data <- cleaned_versions  # Extract the value from the reactive expression
    
    # Filter the dataset based on selected item and page
    filtered_data_wrong <- cleaned_data %>%
      filter(verb %in% c("submitted", "answered", "selected")) %>%
      select(itemCreditAchieved, userId, response, responseText, item, componentName, pageNumber) %>%
      filter(componentName != "/aboutSelf" & !is.na(pageNumber) & !is.na(item) & !is.na(responseText)) %>% 
      filter(responseText != "NULL" & responseText != "＿") %>%
      filter(itemCreditAchieved < 1) %>%
      filter(item == gsub("Item ", "", item_selected)) %>%
      filter(pageNumber == gsub("Page ", "", page_selected)) %>%
      group_by(responseText) %>%  
      summarise(n = n()) %>%  
      filter(n >= 10) %>%
      filter(n %in% (0:slider_selected)) %>%
      ungroup() %>% 
      mutate(responseText = fct_reorder(
        as.character(responseText),
        n,
        .desc = TRUE
      ) %>% fct_rev())
    
    if (nrow(filtered_data_wrong) == 0) {
      # Create an empty plot if no data points are available
      return(plot_ly(x = NULL, y = NULL, type = "bar"))
    } else {
      # Generate the plot using ggplot2
      return(plot_ly(data = filtered_data_wrong, 
                     x = ~n, 
                     y = ~responseText, 
                     type = "bar") %>%
               layout(
                 yaxis = list(title = "Wrong Answer"),  # Swap x-axis and y-axis titles
                 xaxis = list(title = "Frequency (if more than 10 times)"),  # Swap x-axis and y-axis titles
                 title = paste("Graph of",
                               page_selected,
                               "With",
                               item_selected),
                 barmode = "stack"  # Add this line to adjust bar mode if needed
               ))
      
    }
  })
  
}

shinyApp(ui, server)
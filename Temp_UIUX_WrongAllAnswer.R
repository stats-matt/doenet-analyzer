library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)
library(dplyr)
library(DT)
library(promises)
library(future)
library(ggplot2)

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
    column(6, selectInput("page_dropdown", "Select Page", "")),
    column(6, selectInput("item_dropdown", "Select Item", ""))
  ),
  sliderInput("integer", "Top 10 Most Frequent Wrong Answers",
              min = 0, 
              max = 0,
              value = 0),
  # Render the plot
  plotOutput("wrong_plot")
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
  # Observe changes in the item dropdown and update choices for page dropdown accordingly
  observeEvent(input$item_dropdown, {
    item_choices <- facet_naming_item()
    updateSelectInput(session, "item_dropdown", choices = item_choices)
    page_choices <- facet_naming_page()
    updateSelectInput(session, "page_dropdown", choices = facet_naming_page())
  })
  # Observe changes in the item and page dropdowns
  max_value_sliderdf <- reactiveVal(0)  # Use a reactiveVal to store the max value
  observe({
    item_selected <- input$item_dropdown
    page_selected <- input$page_dropdown
    
    a <- cleaned_versions %>%
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
    
    if (nrow(a) > 0) {
      max_value_sliderdf(max(a$n))  # Update the reactive value
      # Update the sliderInput dynamically
      updateSliderInput(session, "integer", max = max_value_sliderdf(), value = max_value_sliderdf())
    } else {
      max_value_sliderdf(0)  # Reset the reactive value to 0
      # If 'a' is empty, set the maximum and default value of the slider to 0 or any other appropriate default value
      updateSliderInput(session, "integer", max = 0, value = 0)
    }
  })
  # Inside the output$wrong_plot renderPlot function
  output$wrong_plot <- renderPlot({
    item_selected <- input$item_dropdown
    page_selected <- input$page_dropdown
    slider_selected <- input$integer
    
    cleaned_data <- cleaned_versions  # Extract the value from the reactive expression
    
    # Filter the dataset based on selected item and page
    filtered_data <- cleaned_data %>%
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
    
    if (nrow(filtered_data) == 0) {
      # Display a message to the user if no data points for the selected combination
      return(plot(0, main = "No data for selected combination",
                  xlab = "", 
                  ylab = ""))
    } else {
      # Generate the plot using ggplot2
      ggplot(filtered_data, aes(x = responseText, y = n)) +
        geom_col() +
        labs(x = "Wrong Answer", 
             y = "Frequency (if more than 10 times)",
             title = paste("Graph of",
                           page_selected,
                           "With",
                           item_selected)) +
        coord_flip() +
        theme(
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5)
        )
    }
  })
  
}

shinyApp(ui, server)
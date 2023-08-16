# The following are graphs that I have chosen from cloudword_temp.R:
# 1) Graph 1
# 2) Graph 4
# 3) World Cloud 2

# class size divided by 4 for filter n-count 
# class size = unique(userids)
# include slider with the value (max) above^ min of 1, max of divided by 4
# make the wordcloud plotly
# server integration

# fix plotly for all answers
# implemenet it into wrongasnwers
# server integration
# cord flip for all graphs (wrong and all answer)



library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)
library(tidytext)
library(igraph)
library(ggraph)
library(wordcloud)
library(tm)  # Add the tm package for pairwise_count



doenetid <- "_pdiqrEQqDLsTCucSaMdw1"
raw <- stream_in(file(
  paste0(
    "https://www.doenet.org/api/getEventData.php?doenetId[]=",
    doenetid
  )
))

# clean the data, test the functions
events <- raw$events[[1]]
dates <- pull_dates(events)
cleaned_versions <- clean_events(events, min(dates), max(dates))

ui <- fluidPage(
  titlePanel("Doenet Data Analysis"),
  mainPanel(
    tabsetPanel(
      tabPanel("Graph 1", plotOutput("graph1")),
      tabPanel("Graph 4", plotOutput("graph4")),
      tabPanel("Word Cloud 2", plotOutput("wordcloud2"))
    ),
    sliderInput("wordcloud_int", 
                "Class size",
                min = 1,
                max = 0,
                value = 0),
    selectInput("componentType_dropdown", 
                "Select Type Of Question",
                choices = c("answer",
                            "choiceInput",
                            "textInput"))
  )
)

server <- function(input, output, session) {
  
  # DONE
  # class size divided by 4 for filter n-count 
  # class size = unique(userids)
  # include slider with the value (max) above^
  
  class_size <- function() {
    fitlered_classSize <- cleaned_versions %>%
      filter(verb %in% c("submitted", "answered", "selected")) %>%
      group_by(userId) %>%
      count(userId)
    
    classSize_cal <- max(fitlered_classSize$n)/4
    return(classSize_cal)
  }
  
  observe({
    updateSliderInput(session,
                      "wordcloud_int", 
                      max = class_size(), 
                      value = class_size())
    
  })
  
  # Process the data for Graph 1
  filtered_text1 <- reactive({
    
    select_input <- input$componentType_dropdown
    
    cleaned_versions %>%
      filter(verb %in% c("submitted", "answered", "selected")) %>%
      filter(componentType == paste0("", select_input)) %>%
      select(userId, pageNumber, responseText) %>%
      filter(!is.null(responseText)) %>%
      mutate(responseText = as.character(responseText)) %>%
      # past0 is not valid, and just have (-1E6:1E6) as.char(-1E6:1E6) as.character(1:5)
      filter(!responseText %in% c('NULL', ' ', paste0("", (-1E6:1E6)))) %>%
      select(userId, pageNumber, responseText) %>%
      unnest_tokens(word, responseText) %>% 
      anti_join(stop_words) %>%
      filter(!word %in% c(paste0("", (-1E6:1E6)))) %>%
      widyr::pairwise_count(word, userId, sort = TRUE, upper = FALSE)  # Use the tm package here
  })
  
  # Process the data for Graph 2
  filtered_text2 <- reactive({
    
    select_input <- input$componentType_dropdown
    
    cleaned_versions %>%
      filter(verb %in% c("submitted", "answered", "selected")) %>%
      filter(componentType == paste0("", select_input)) %>%
      select(userId, pageNumber, responseText) %>%
      filter(!is.null(responseText)) %>%
      mutate(responseText = as.character(responseText)) %>%
      filter(!responseText %in% c('NULL', ' ', paste0("", (-1E6:1E6)))) %>%
      select(userId, pageNumber, responseText) %>%
      widyr::pairwise_count(responseText, userId, sort = TRUE, upper = FALSE)  # Use the tm package here
  })
  
  # Render the Plotly graph using filtered_data1
  output$graph1 <- renderPlot({
    
    slider_classSize <- input$wordcloud_int
    
    filtered_data1 <- filtered_text1() %>%
      filter(n %in% (0:slider_classSize))
    
    if (nrow(filtered_data1) == 0) {
      # If no data passes the filter, create a default plot
      default_plot <- plot_ly() %>%
        add_text(
          x = 0.5, y = 0.5,
          text = "No available data to analyze",
          font = list(size = 30, color = "red")
        )
      return(default_plot)
    } else {
      # Create the graph using filtered_data1
      graph <- filtered_text1() %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
        geom_node_point(size = 5) +
        geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
        theme_void()
      
      return(graph)
    }
  })
  
  output$graph4 <- renderPlot({
    
    slider_classSize <- input$wordcloud_int
    
    filtered_data2 <- filtered_text2() %>%
      filter(n %in% (0:slider_classSize))
    
    if (nrow(filtered_data2) == 0) {
      # If no data passes the filter, create a default plot
      default_plot <- ggplot() +
        geom_text(aes(x = 0.5, y = 0.5, label = "No available data to analyze"),
                  size = 5, color = "red", hjust = 0.5, vjust = 0.5) +
        theme_void()
      return(default_plot)
    } else {
      # Create the graph using filtered_data2
      graph <- filtered_data2 %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
        geom_node_point(size = 5) +
        geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
        theme_void()
      return(graph)
    }
  })
  
  output$wordcloud2 <- renderPlot({
    
    slider_classSize <- input$wordcloud_int
    
    filtered_data2 <- filtered_text2() %>%
      filter(n %in% (0:slider_classSize))
    
    if (nrow(filtered_data2) == 0) {
      # If no data passes the filter, create a default text annotation
      default_plot <- ggplot() +
        geom_text(aes(x = 0.5, y = 0.5, label = "No available data to analyze"),
                  size = 5, color = "red", hjust = 0.5, vjust = 0.5) +
        theme_void()
      return(default_plot)
    } else {
      word <- wordcloud(filtered_data2, scale = c(2, 1), min.freq = 50, colors = rainbow(30))
      
      return(word)
    }
  })
  
}

shinyApp(ui, server)
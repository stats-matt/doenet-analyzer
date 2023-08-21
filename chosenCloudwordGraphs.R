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




#doenetid <- "_pdiqrEQqDLsTCucSaMdw1"
doenetid <- "_ds4TPv9RAI9b3DnYvgROo"
raw <- stream_in(file(
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
  titlePanel("Doenet Data Analysis"),
  mainPanel(
    "Analyze Free Response Questions (FRQ)",
    "The following tabs are a quick overview of free response questions (FRQ)",
    p("How Do I Interpret The Graphs?",
      "The subsequent graphs are employed to analyze \"unstructured\" text 
        found in free-response questions. \"Graph 1\" and \"Graph 2\" are 
        webs/networks that illustrate the connections between words or answers 
        within a free-response question (FRQ). The strength of the connection 
        between words or answers in the network is indicated by the color cyan. 
        The darker the color, the stronger the connection. The color of the 
        connection also represents the frequency with which a word has been 
        linked to others. For instance, pastel cyan might indicate that the 
        words were linked three times, while dark cyan could indicate a 
        connection frequency of 2000 times. \"Word Cloud\" is a collection 
        of words that depicts the occurrences of words or answers. 
        The larger the word or answer, the more common it is for that term to 
        appear. Conversely, the smaller the word or answer, the rarer the 
        likelihood of encountering it."),
    h4("NOTES:"),
    tags$ol(
      tags$li("\"Graph 1\", \"Graph 2\", and the \"Word Cloud\" graphs are 
                distinct plots that analyze the data differently. Although 
                \"Graph 1\" and \"Graph 2\" might appear similar, \"Graph 1\" 
                excludes common words typically found in the English language; 
                words like \"and\", \"I\", \"it\", \"can\", etc., are all 
                omitted in \"Graph 1\", whereas \"Graph 2\" includes these 
                words."),
      tags$li("The class size slider does not directly reflect the number of 
                students in a class. Instead, it is used during the calculation 
                of relationships between each word."),
      tags$li("The dropdown menu includes only three question types based on 
                the tags associated with the questions at the time of coding. 
                These three types are related to Free-Response Questions (FRQs), 
                thus they are included in the dropdown menu."),
      tags$li("The graphs are not intended for grading FRQs; rather, they 
                provide a rapid overview of common terms or words in students' 
                answers within an FRQ.")
    ),
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
                choices = c("Answer",
                            "Choice Input",
                            "Text Input"))
  )
)

server <- function(input, output, session) {
  
  # Define a function to map dropdown selections to dataset operations
  map_dropdown_to_operation <- function(selected_value) {
    switch(selected_value,
           "Answer" = "answer",
           "Choice Input" = "choiceInput",
           "Text Input" = "textInput",
           stop("Invalid selection"))
  }
  
  # Calculate class size
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
    
    component_type_selected <- input$componentType_dropdown
    
    operation <- map_dropdown_to_operation(component_type_selected)

    cleaned_versions %>%
      filter(verb %in% c("submitted", "answered", "selected")) %>%
      filter(componentType == operation) %>%
      select(userId, pageNumber, responseText) %>%
      filter(!is.na(responseText) & !is.null(responseText)) %>%
      mutate(responseText = as.character(responseText)) %>%
      filter(!responseText %in% c('NULL', ' ', as.character(-1E6:1E6))) %>%
      select(userId, pageNumber, responseText) %>%
      unnest_tokens(word, responseText) %>% 
      anti_join(stop_words) %>%
      filter(!word %in% c(as.character(-1E6:1E6))) %>%
      widyr::pairwise_count(word, userId, sort = TRUE, upper = FALSE)
  })
  
  # Process the data for Graph 2
  filtered_text2 <- reactive({
    
    component_type_selected <- input$componentType_dropdown
    
    operation <- map_dropdown_to_operation(component_type_selected)
    
    cleaned_versions %>%
      filter(verb %in% c("submitted", "answered", "selected")) %>%
      filter(componentType == operation) %>%
      select(userId, pageNumber, responseText) %>%
      filter(!is.na(responseText) & !is.null(responseText)) %>%
      mutate(responseText = as.character(responseText)) %>%
      filter(!responseText %in% c('NULL', ' ', as.character(-1E6:1E6))) %>%
      select(userId, pageNumber, responseText) %>%
      widyr::pairwise_count(responseText, userId, sort = TRUE, upper = FALSE)
  })
  
  # Render the Plotly graph using filtered_data1
  output$graph1 <- renderPlot({
    
    slider_classSize <- input$wordcloud_int
    
    filtered_data1 <- filtered_text1() %>%
      filter(n %in% (0:slider_classSize))
    
    if (nrow(filtered_data1) == 0) {
      # If no data passes the filter, create a default plot
      default_plot <- ggplot() +
        geom_text(aes(x = 0.5, y = 0.5, label = "No available data to analyze"),
                  size = 5, color = "red", hjust = 0.5, vjust = 0.5) +
        theme_void()
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
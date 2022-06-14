library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)
library(dplyr)
library(tidytext)

shinyServer(function(input, output) {
 
  source("functions.R")
  #This block pulls out the events log, which is a dataframe, within a 
  # 1 element list within a 1 by 3 dataframe. So df is the frame,
  # events is an named column of df, which contains one element, which is a 
  # dataframe containing the events log, which we are then assigning to a local
  # variable called events. Note the difference between the events column of df
  # and our local events object (even though they are essentially the same data)

     df <- eventReactive(input$update, {
       stream_in(file(

           "https://www.doenet.org/api/getEventData.php?doenetId[]=_dKAX4QFX3JGXILGwaApZY"
           
         
       ))
     })
  
  
  events <- reactive({
    df()$events[[1]]
  })
  
  
  
  # Takes our events and cleans them up and adds some helpful columns
  # See file functions.R for more information.
  cleaned <- reactive({

    clean_events(events())
    
  })
  
  summarized = reactive({summarize_events(cleaned())
    
    })
  
  output$raw <- renderDataTable(events())
  output$cleaned_data <- renderDataTable(cleaned())
  output$summarized_data <- renderDataTable(summarized())
  
  
  
  output$graph3 <- renderPlot(
    summarized() %>%
      group_by(userId) %>%
      mutate(total = sum(score)) %>%
      ggplot(aes(x = total)) +
      geom_histogram() +
      labs(x = "Total Points", y = "Number of Students", title = "Total Scores on Assignment") + 
      facet_wrap( ~ version_num, labeller = label_both) 
  )
  
  output$graph4 <- renderPlot(
    
    
    summarized() %>% group_by(version_num)
    %>% ggplot(aes(x = problem, y = avg))
    
    +geom_col()
    +facet_wrap( ~ version_num, labeller = label_both, scales = "free_y")
    +ylim(c(0,1))
  )
  
})

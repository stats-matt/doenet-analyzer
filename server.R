library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)

shinyServer(function(input, output) {
  source("./functions.R")
  
  # I SHOULD USE COMMENTS!!!!!
  
  
  # What this code is doing is pulling in the data
  # getQueryString() is a function that takes a query string and turns it into
  # a list, which allows us to find the "data" item of that list.
  # By default it pulls the query string from the app environment (?)
  # renderText turns it that list into a string and then checks if it is null
  # This is a check to make sure we are in fact looking for data that exists

  #Stream_in unpacks the json file we get from the URL into a 1 by 3 dataframe 
  #First element is a boolean that tells if it was successful or not
  #Second element is a message (typically empty right now)
  #Third is the list that contains the event log
  #df contains this 1 by 3 frame at the end of this block
  
  
  # if (!is.null(renderText(getQueryString()[["data"]]))) {
  #   df <- eventReactive(input$update, {
  #     stream_in(file(
  #       paste0(
  #         "https://www.doenet.org/api/getEventData.php?doenetId[]=",
  #         getQueryString()[["data"]]
  #       )
  #     ))
  #   })
  
  
    
   link = "https://www.doenet.org/api/getEventData.php?doenetId[]=_YImZRcgrUqyNBLHd0tbP2"
   df <- eventReactive(input$submit_extra | input$update, {
      if(input$submit_extra != 0){
        link = paste0(link, "&doenetId[]=",input$extra_id)
        }
     stream_in(file(link))
    })
    
    #get set dataset (for testing)
    # df <- eventReactive(input$update, {
    #   
    # })
    
    
    
    #This block pulls out the events log, which is a dataframe, within a 
    # 1 element list within a 1 by 3 dataframe. So df is the frame,
    # events is an named column of df, which contains one element, which is a 
    # dataframe containing the events log, which we are then assigning to a local
    # variable called events. Note the difference between the events column of df
    # and our local events object (even though they are essentially the same data)
    
    events <- reactive({
      df()$events[[1]]
    })
    
  
    # Takes our events and cleans them up and adds some helpful columns
    # See file functions.R for more information.
    cleaned <- reactive({
      clean_events(events())
    })
    
    # creates a table of cleaned data
    output$cleaned_data <- renderDataTable(cleaned())
    
    # creates a table of raw data
    output$raw <- renderDataTable(events())
    
    #creates an output text detailing how many students in the data set
    output$num_students <-
      renderText(paste0(
        "There is/are ",
        n_distinct(events()$userId, na.rm = TRUE),
        " student(s)"
      ))
    
    #creates an output text detailing how many different doenet experiments 
    #are represented in this set.
    output$num_doenetIds <-
      renderText(paste0(
        "There is/are ",
        n_distinct(events()$doenetId, na.rm = TRUE),
        " doenetId(s)"
      ))
    #creates an output text detailing how many pages are included in this dataset
    output$num_pages <-
      renderText(paste0(
        "There is/are ",
        n_distinct(summary_data()$pageNumber, na.rm = TRUE),
        " page(s)"
      ))
    
    #This creates the summary data
    #We start with the cleaned data and select the relevant columns
    #Then we group by userID and pageNumber
    #Then the summarize_all gives us the max value (so the max time it took to 
    # answer that problem on that page, or something like that)
    # pivot_longer sets up a column with all the problem names (drawn from the 
    # X1 - Xn columns in the cleaned set) and then takes the values from those 
    # Columns and drops them into the score column of the summary
    
    #A brief note about group_by and ungroup - it is my understanding that if you
    # do not ungroup, the next time you group it will group by both the first 
    # parameter you called it with and then the second one as well, and as far
    # as I know this persists across function calls, so it is crucial that we 
    # ungroup!
    
    summary_data <- reactive({
      cleaned() %>%
        select(userId, starts_with("X"), time, timestamp, pageNumber) %>%
        group_by(userId, pageNumber) %>%
        summarize_all("max", na.rm = T) %>%
        pivot_longer(cols = starts_with("X"),
                     names_to = "problem",
                     values_to = "score") %>%
        ungroup() %>%
        filter(score != -Inf)
    })
    
    # This renders the summary data in a table
    output$summary <- renderDataTable(summary_data())
    
    
    #This gives allows the user to download the data shown in a csv file for their
    #own purposes
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('events-', Sys.Date(), '.csv', sep = '')
      },
      content = function(file) {
        write.csv(events(), file)
      }
    )
    
    #This displays a series of histograms for scores on each problem on each page
    output$hist_prob <- renderPlot(
      summary_data() %>%
        ggplot(aes(x = score)) +
        geom_histogram() +
        facet_grid(pageNumber ~ problem) +
        labs(x = "Score on Problem", y = "Count", title = "Breakdown by Problem")
    )
    #This displays a histogram of overall scores on the activity
    output$hist_total <- renderPlot(
      summary_data() %>%
        group_by(userId) %>%
        summarize(total = sum(score)) %>%
        ggplot(aes(x = total)) +
        geom_histogram() +
        labs(x = "Total Points", y = "Number of Students", title = "Total Scores on Assignment")
    )
    #This is a plot that shows time to credit for each problem
    #TODO - make this able to show depending on assigned time or started time
    output$time_plot <- renderPlot({
    cleaned() %>%
      filter(!is.na(itemCreditAchieved)) %>% 
      ggplot(aes(y = itemCreditAchieved, x = time, color=userId))+
      geom_line()+
      theme(legend.position = "none")+
      facet_wrap(~pageNumber)+
      labs(x = "Time", y = "Total Credit on Page")
    })
  })



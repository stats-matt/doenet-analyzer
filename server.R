library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)
library(dplyr)

shinyServer(function(input, output) {
  source("./functions.R")
  
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
  
  # A quick note for anyone reading this that relates to the work we are doing
  # on being able to import multiple data sets - the website does not allow
  # you to import the same data set twice - I tried, it doesn't work, and that
  # is not coming from us thats coming from somewhere higher up the chain, I am
  # guessing thats from the server? Is this something we need to look into?
  
  
  # if (!is.null(renderText(getQueryString()[["data"]]))) {
  #   df <- eventReactive(input$update, {
  #     stream_in(file(
  #       paste0(
  #         "https://www.doenet.org/api/getEventData.php?doenetId[]=",
  #         getQueryString()[["data"]]
  #       )
  #     ))
  #   })
  
  #maximum 5 ids to compare/ hard coded
  #So this code is going through and adding one extra Doenet ID entry box
  # for each one requested in input$numid. If (for example) you only request
  # two, then the remaining 3 output IDs will be set to NULL and not shown
  # It feels like we could accomplish this with a loop, but after multiple
  # attempts, none of us have been successful.
  output$rid <-
    renderUI({
      i = 1
      j = input$numid
      if (i <= j) {
        output$id1 <- renderUI({
          textInput("id1", "Doenet ID 1")
        })
        i = i + 1
      } else{
        output$id1 = NULL
      }
      if (i <= j) {
        output$id2 <- renderUI({
          textInput("id2", "Doenet ID 2")
        })
        i = i + 1
      } else{
        output$id2 = NULL
      }
      if (i <= j) {
        output$id3 <- renderUI({
          textInput("id3", "Doenet ID 3")
        })
        i = i + 1
      } else{
        output$id3 = NULL
      }
      if (i <= j) {
        output$id4 <- renderUI({
          textInput("id4", "Doenet ID 4")
        })
        i = i + 1
      } else{
        output$id4 = NULL
      }
      if (i <= j) {
        output$id5 <- renderUI({
          textInput("id5", "Doenet ID 5")
        })
        i = i + 1
      } else{
        output$id5 = NULL
      }
    })
  
  #link = "https://www.doenet.org/api/getEventData.php?doenetId[]=_YImZRcgrUqyNBLHd0tbP2"
  
  df <- eventReactive(input$submit_extra | input$update, {
    #if (input$submit_extra != 0) {
      #TRYING to check each ids, right now not working
      # link = paste0(link,"&doenetId[]=", input$id1)
      # if (input$id2 != "" | NULL){
      #   link = paste0(link, "&doenetId[]=",input$id2)
      # }
      # if (input$id3 != "" | NULL){
      #   link = paste0(link, "&doenetId[]=",input$id3)
      # }
      # if (input$id4 != "" | NULL){
      #   link = paste0(link, "&doenetId[]=",input$id4)
      # }
      # if (input$id5 != ""| NULL){
      #   link = paste0(link, "&doenetId[]=",input$id5)
    #   end_of_link = paste0(
    #     "&doenetId[]=",
    #     input$id1,
    #     "&doenetId[]=",
    #     input$id2,
    #     "&doenetId[]=",
    #     input$id3,
    #     "&doenetId[]=",
    #     input$id4,
    #     "&doenetId[]=",
    #     input$id5
    #   )
    #   
    # }
    # else{
    #   end_of_link = ""
    # }
    stream_in(file(
      paste0(
        "https://www.doenet.org/api/getEventData.php?doenetId[]=",
        #"_YImZRcgrUqyNBLHd0tbP2" # for debugging to have a set doenetid to use
        getQueryString()[["data"]]
        #end_of_link
      )
    ))
  })
  
  #INSERT THIS FOR WEB VERSION
  #getQueryString()[["data"]],
  
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
  
  #slider of the time for the graph
  #max could be the total allowed time for the assignment if there is a way to
  #get.
  output$slider <-
    renderUI({
      sliderInput(
        "maxtime",
        "Maximum time shown:",
        min = 0,
        max = input$maxtime_set,
        value =  c(500, 10000)
      )
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
    
    #This displays a plot of average submissions per question    
    output$hist_submissions <- renderPlot({
      submitted_data <- function(){cleaned()[cleaned()$verb=="submitted",]}
      totals <- table(submitted_data()$componentName)/n_distinct(events()$userId, na.rm = TRUE)
      ggplot(as.data.frame(totals), aes(x=Var1, y=Freq)) +
        geom_bar(stat="identity") +
        labs(x="Question", y="Submissions", title = "Average Number of Submissions per Question")
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
    summarize_events(cleaned())
  })
    
  #This is a plot that shows time to credit for each problem
  output$time_plot <- renderPlot({
    cleaned() %>%
      filter(!is.na(itemCreditAchieved)) %>%
      ggplot(aes(y = itemCreditAchieved, x = time, color = userId)) +
      geom_line() +
      theme(legend.position = "none") +
      facet_wrap( ~ pageNumber) +
      labs(x = "Time", y = "Total Credit on Page") +
      xlim(input$maxtime[1], input$maxtime[2])
  })
  
  output$time_plot_s <- renderPlot({
    cleaned() %>%
      filter(!is.na(itemCreditAchieved)) %>%
      ggplot(aes(y = itemCreditAchieved, x = time, color = userId)) +
      geom_line() +
      theme(legend.position = "none") +
      facet_wrap( ~ pageNumber) +
      labs(x = "Time", y = "Total Credit on Page") +
      xlim(0, input$maxtime[2])
  })
  output$version_version <- renderPlot({
    summary_data() %>%
      group_by(version_num) %>%
      #arrange(avg) %>%
      ggplot(aes(
        x = problem,
        y = avg,
        fill = as.factor(version_num)
      )) +
      geom_col(stat = "identity", position = 'dodge') +
      labs(x = "problem", y = "average score", title = "average score by problem by version") +
      guides(fill = guide_legend(title = "Version")) +
      ylim(c(0, 1))
  })
  
})

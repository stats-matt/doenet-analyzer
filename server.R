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
  
  # ==========PRE CLEANING INPUTS==========================
  
  # Slider for time in the time plots
  output$time_slider <-
    renderUI({
      sliderInput(
        "maxtime",
        min = 0,
        "Maximum time shown:",
        max = input$maxtime_set,
        value =  c(500, 10000)
      )
    })
  
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
  
  query <- reactive({
    getQueryString()
  })
  
  output$show_pulldown <- reactive({
    length(unlist(query())) > 1
  })
  outputOptions(output, "show_pulldown", suspendWhenHidden = FALSE)
  
  observe({
    updateSelectInput(
      session = session,
      inputId = "activity_select",
      choices = c("All", paste(query())),
    )
  })
  
  events_full <- reactive({
    load_data(query())
  })
  
  events <- reactive({
    if (input$activity_select == "All") {
      events_full()
    }
    else{
      events_full() %>% filter(doenetId == input$activity_select)
    }
  })
  
  #if (observe(length(unlist(query()) > 1))) {
  #  doenetId_list <- reactive({1:length(unlist(query()))})
  #names(doenetId_list()) <- reactive({events()$doenetId %>% unique()})
  #}
  
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
  
  
  
  # Input from date slider determines which dates are included in the set.
  cleaned_versions <- reactive({
    #req(events())
    withProgress(message = 'Analyzing Data', {
      clean_events(events(), input$date_range[1], input$date_range[2])
    })
  })
  
  summary_data_versions <- reactive({
    summarize_events(cleaned_versions())
  })
  
  # Filter takes in previously cleaned data and then the version we select
  cleaned <- reactive({
    version_filter(cleaned_versions(), input$version_selected)
  })
  
  summary_data <- reactive({
    summarize_events(cleaned())
  })
  
  # These next two lines pull data directly from events (before it is cleaned)
  # that is crucial to determining the date and version selection on the sidebar.
  # As a rule we have typically tried to avoid working on events directly, but
  # because this determines how we clean we have made an exception.
  dates <- reactive(pull_dates(events()))
  versions <- reactive(pull_versions(events()))
  
  # This outputs the version selection and the date slider for the UI
  output$version_select <- renderUI({
    selectInput("version_selected", "Version: ", c(1:versions()))
  })
  output$date_slider <- renderUI({
    sliderInput(
      "date_range",
      "Data from: ",
      min = min(dates()),
      max = max(dates()),
      value = c(min(dates()),
                max(dates()))
    )
  })
  
  
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
  
  # =========================DATA TABLES===========================================
  # creates tables of each of the data versions to view/troubleshoot in a tab
  output$events_dt <-
    DT::renderDT(events())
  output$cleaned_versions_dt <-
    DT::renderDT(cleaned_versions())
  output$summary_data_versions_dt <-
    DT::renderDT(summary_data_versions())
  output$cleaned_dt <-
    DT::renderDT(cleaned())
  output$summary_data_dt <-
    DT::renderDT(summary_data())
  
  # =======================SUMMARY TEXT============================================
  # creates an output text detailing how many students in the data set
  output$num_students <-
    renderText(paste0(
      "There is/are ",
      n_distinct(events()$userId, na.rm = TRUE),
      " student(s)"
    ))
  # creates an output text detailing how many versions are present in the set
  output$num_versions <-
    renderText(paste0("There are ", versions()))
  
  # creates an output text detailing how many different doenet experiments
  # are represented in this set.
  output$num_doenetIds <-
    renderText(paste0(
      "There is/are ",
      n_distinct(events()$doenetId, na.rm = TRUE),
      " doenetId(s)"
    ))
  # creates an output text detailing how many pages are included in this dataset
  output$num_pages <-
    renderText(paste0(
      "There is/are ",
      n_distinct(summary_data_versions()$pageNumber, na.rm = TRUE),
      " page(s)"
    ))
  
  # =============================GENERAL PLOTS=====================================
  # This is a plot that shows time to credit for each problem
  output$time_plot_a <- renderPlot({
    cleaned() %>%
      filter(!is.na(itemCreditAchieved)) %>%
      ggplot(aes(y = itemCreditAchieved, x = time, color = userId)) +
      geom_step() +
      theme(legend.position = "none") +
      facet_wrap( ~ pageNumber) +
      labs(x = "Time", y = "Total Credit on Page") +
      xlim(input$maxtime[1], input$maxtime[2])
  })
  # This is the time plot from the start (start point in time is always 0)
  output$time_plot_s <- renderPlot({
    cleaned() %>%
      filter(!is.na(itemCreditAchieved)) %>%
      ggplot(aes(y = itemCreditAchieved, x = time, color = userId)) +
      geom_step() +
      theme(legend.position = "none") +
      facet_wrap( ~ pageNumber) +
      labs(x = "Time", y = "Total Credit on Page") +
      xlim(0, input$maxtime[2])
  })
  
  # This displays a series of histograms for scores on each problem on each page
  output$hist_prob <- renderPlot(
    # bins = nrow(distinct(summary_data() , score))
    summary_data() %>%
      filter(!is.na(itemCreditAchieved)) %>%
      group_by(userId, pageNumber, item) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      ggplot(aes(x = itemCreditAchieved)) +
      geom_histogram() +
      facet_grid(pageNumber ~ item) +
      labs(x = "Score on Problem", y = "Count", title = "Breakdown by Problem")
  )
  # This displays a histogram of overall scores on the activity
  # bins = nrow(distinct())
  output$hist_total <- renderPlot({
    shiny::validate(
      need(
        summary_data(),
        "Sorry, there is no data for you requested combination.
                      Please change your input selections"
      )
    )
    
    summary_data() %>%
      group_by(userId, pageNumber) %>%
      summarize(total = max(pageCreditAchieved, na.rm = TRUE)) %>%
      ggplot(aes(x = total)) +
      geom_histogram() +
      labs(x = "Total Points", y = "Number of Students", title = "Total Scores on Assignment, By Page") +
      facet_wrap(~ pageNumber)
  })
  
  # ========================ATTEMPT BASED PLOTS====================================
  # This displays a plot of average submissions per question
  output$hist_submissions <- renderPlot({
    submitted_data <- cleaned() %>% filter(verb == "submitted")
    totals <-
      as.data.frame.table(table(submitted_data$componentName) / n_distinct(events()$userId, na.rm = TRUE))
    ggplot(totals, aes(x = Var1, y = Freq)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(breaks = pretty_breaks()) +
      labs(x = "Question", y = "Submissions", title = "Average Number of Submissions per Question (All Attempts)") +
      coord_flip()
  })
  
  # This displays a plot of how the submissions are distributed across attempts
  output$hist_subm_attempt <- renderPlot({
    submitted_data <- cleaned() %>% filter(verb == "submitted")
    if (input$MeanVar == "cm") {
      #cumulative submissions per question
      ggplot(submitted_data,
             aes(x = componentName, fill = attemptNumber)) +
        geom_bar(position = "dodge") +
        scale_y_continuous(breaks = pretty_breaks()) +
        labs(x = "Question", y = "Number of Submissions", title = "Number of Submissions Across Attempts") +
        guides(fill = guide_legend(title = "Attempt Number"))
    } else {
      #average submissions per question
      totals <-
        table(submitted_data$componentName,
              submitted_data$attemptNumber) %>% as.data.frame.table()
      colnames(totals) <- c("Question", "AttemptN", "Submissions")
      for (i in 1:max(submitted_data$attemptNumber)) {
        #divide cumulative submissions by the number of students submitting in each attempt number
        subData <- submitted_data %>% filter(attemptNumber == i)
        totals[totals$AttemptN == i, 3] <-
          totals[totals$AttemptN == i, 3] / n_distinct(subData$userId, na.rm = TRUE)
      }
      ggplot(totals, aes(x = Question, y = Submissions, fill = AttemptN)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_y_continuous(breaks = pretty_breaks()) +
        labs(x = "Question", y = "Submissions", title = "Average Number of Submissions Across Attempts") +
        guides(fill = guide_legend(title = "Attempt Number"))
    }
  })
  
  # This displays a plot of how the submissions are distributed across versions
  output$hist_subm_version <- renderPlot({
    submitted_data <- cleaned() %>% filter(verb == "submitted")
    if (input$MeanVar == "cm") {
      #cumulative submissions
      ggplot(submitted_data, aes(x = componentName, fill = as.factor(version_num))) +
        geom_bar(position = "dodge") +
        scale_y_continuous(breaks = pretty_breaks()) +
        labs(x = "Question", y = "Number of Submissions", title = "Number of Submissions Across Versions") +
        guides(fill = guide_legend(title = "Version Number"))
    } else {
      #average submissions per version
      totals <-
        table(submitted_data$componentName,
              submitted_data$version_num) %>% as.data.frame.table()
      colnames(totals) <- c("Question", "VersionN", "Submissions")
      for (i in unique(submitted_data$version_num)) {
        subData <- submitted_data %>% filter(version_num == i)
        totals[totals$VersionN == i, 3] <-
          totals[totals$VersionN == i, 3] / n_distinct(subData$userId, na.rm = TRUE)
      }
      ggplot(totals, aes(x = Question, y = Submissions, fill = VersionN)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_y_continuous(breaks = pretty_breaks()) +
        labs(x = "Question", y = "Submissions", title = "Average Number of Submissions Across Versions") +
        guides(fill = guide_legend(title = "Version Number"))
    }
  })
  
  # ========================QUESTION SPECIFIC PLOTS====================================
  # This displays a plot of the submission percentiles for a specific question
  output$q_submissions <- renderPlot({
    q_data <-
      cleaned() %>% filter(verb == "submitted", componentName == input$subm_q)
    n_subm_by_id <- table(q_data$userId) %>% as.data.frame()
    ggplot(n_subm_by_id, aes(x = Freq)) +
      geom_bar(stat = "count") +
      scale_y_continuous(breaks = pretty_breaks()) +
      labs(x = "Number of Submissions", y = "Number of Students", title = "Distribution of Submissions")
  })
  
  # This displays a pie chart of how many students submitted, solved, and did not attempt a problem
  output$q_pie <- renderPlot({
    q_data <-
      cleaned() %>% filter(verb == "submitted", componentName == input$subm_q)
    subm_by_id <-
      table(q_data$userId, q_data$creditAchieved) %>% as.data.frame()
    solv <-
      nrow(subm_by_id[subm_by_id$Var2 == 1 & subm_by_id$Freq > 0, ])
    sub <- n_distinct(subm_by_id$Var1) - solv
    not_att <-
      n_distinct(events()$userId, na.rm = TRUE) - solv - sub
    results <-
      data.frame(
        Legend = c("solved", "unsolved", "not attempted"),
        num = c(solv, sub, not_att)
      )
    ggplot(results, aes(x = "", y = num, fill = Legend)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(x = "", y = "", title = "Number of Students Solving This Question")
  })
  
  # This displays a dot plot of student scores vs number of submissions on a question
  output$score_dot <- renderPlot({
    q_data <-
      cleaned() %>% filter(verb == "submitted", componentName == input$subm_q)
    subm_by_id <- table(q_data$userId) %>% as.data.frame()
    for (i in 1:nrow(subm_by_id)) {
      id <- subm_by_id[i, 1]
      max_score <-
        max((q_data[q_data$userId == id, ])$creditAchieved)
      subm_by_id[i, 3] <- max_score
    }
    ggplot(subm_by_id, aes(x = as.factor(Freq), y = V3)) +
      geom_dotplot(binaxis = "y", stackdir = "center") +
      labs(x = "Number of Submissions", y = "Highest Score", title = "Number of Student Submissions vs Score")
  })
  
  # ====================WRONG ANSWER BASED PLOTS===================================
  # From here down is wrong answer code
  output$wrong_plot <- renderPlot({
    cleaned_versions() %>%
      filter(verb == "submitted" |
               verb == "answered" |
               verb == "selected") %>% # selected are choice inputs
      select(
        itemCreditAchieved,
        userId,
        response,
        responseText,
        item,
        componentName,
        pageNumber
      ) %>%
      filter(componentName != "/aboutSelf") %>%
      filter(!is.na(pageNumber)) %>%
      filter(!is.na(item)) %>%
      filter(responseText != "NULL") %>%
      filter(responseText != "＿") %>%
      filter(itemCreditAchieved < 1) %>%
      group_by(pageNumber, item) %>%
      count(responseText) %>%
      filter(n >= 10) %>%
      ungroup() %>%
      ggplot(aes(x = as.character(responseText), y = n)) +
      geom_col() +
      facet_wrap( ~ pageNumber + item, scales = "free") +
      labs(x = "Wrong Answer", y = "Frequency (if more than 10 times)") +
      coord_flip()
    
    # summary_data() %>%
    #   filter(!is.na(response)) %>%
    #   group_by(item) %>%
    #   filter(itemCreditAchieved < 1) %>%
    #   ggplot(aes(
    #     x = as.factor(response),
    #     y = n,
    #     fill = as.factor(response)
    #   )) +
    #   geom_col() +
    #   facet_wrap( ~ item, scales = "free") +
    #   labs(x = "Wrong Answer", y = "Frequency", fill = "Wrong Answer")
  })
  
  # ====================ALL ANSWER PLOTS===================================
  output$all_answers_plot <- renderPlot({
    cleaned_versions() %>%
      filter(verb == "submitted" |
               verb == "answered" |
               verb == "selected") %>% # selected are choice inputs
      select(item, pageNumber, componentName, responseText) %>%
      filter(!is.na(pageNumber)) %>%
      filter(!is.na(item)) %>%
      filter(responseText != "NULL") %>%
      filter(responseText != "＿") %>%
      #unnest(responseText) %>%
      group_by(item, pageNumber) %>%
      
      count(responseText) %>%
      filter(n >= 10) %>%
      ungroup() %>%
      mutate(responseText = fct_reorder(as.character(responseText),
                                        n,
                                        .desc = TRUE) %>% fct_rev()) %>%
      ggplot(aes(x = responseText, y = n)) +
      geom_col() +
      facet_wrap(pageNumber ~ item,
                 scales = "free") +
      labs(x = "Response", y = "Frequency (if more than 10 times)") +
      coord_flip()
  })
  
  
  output$all_answers_text <- DT::renderDT({
    cleaned_versions() %>%
      filter(verb == "submitted" | verb == "answered") %>%
      group_by(userId) %>%
      slice(n()) %>%
      ungroup() %>%
      #filter(componentName == "/aboutSelf") %>%
      select(response)
  })
  
  # summary_data() %>%
  #   filter(!is.na(response)) %>%
  #   group_by(item) %>%
  #   ggplot(aes(
  #     x = as.factor(response),
  #     y = n,
  #     fill = as.factor(response)
  #   )) +
  #   geom_col() +
  #   facet_wrap( ~ item, scales = "free") +
  #   labs(x = "Answer", y = "Frequency", fill = "Answer")
  
  
  
  
  
  
  # ================VERSION COMPARISON PLOTS=====================
  # This one just does a bar graph of average score for each question
  output$problem_avgs_version <- renderPlot({
    summary_data_versions() %>%
      group_by(userId, pageNumber, item) %>%
      filter(!is.na(itemCreditAchieved)) %>%
      slice_max(itemCreditAchieved, n = 1) %>%
      ungroup() %>%
      ggplot(aes(
        x = as.factor(item),
        y = avg,
        fill = as.factor(version_num)
      )) +
      geom_col(position = "dodge") +
      labs(
        x = "Problem",
        y = "Average score",
        title = "Average score by Problem by Version for Each Page",
        fill = "Version"
      ) +
      # guides(fill=guide_legend(title="Version")) +
      ylim(c(0, 1)) +
      #facet_wrap( ~ pageNumber, labeller=label_bquote(.(levels(as.factor(summary_data_versions$pageNumber)))))
      facet_wrap( ~ pageNumber, labeller = label_bquote(Page ~ .(pageNumber)))
  })
  # This is time plots faceted by version for person version of graph
  output$time_plot_person_version <- renderPlot({
    cleaned_versions() %>%
      filter(!is.na(pageCreditAchieved)) %>%
      ggplot(aes(y = pageCreditAchieved, x = time_person, color = userId)) +
      geom_step() +
      theme(legend.position = "none") +
      facet_grid(version_num ~ pageNumber) +
      labs(x = "Time since person loaded page", y = "Total Credit on Page") +
      xlim(input$maxtime[1], input$maxtime[2])
  })
  # Timeplot from start, again, faceted by version
  output$time_plot_activity_version <- renderPlot({
    cleaned_versions() %>%
      filter(!is.na(pageCreditAchieved)) %>%
      ggplot(aes(y = pageCreditAchieved, x = time_activity, color = userId)) +
      geom_step() +
      theme(legend.position = "none") +
      facet_grid(version_num ~ pageNumber) +
      labs(x = "Time since page was first loaded (by anyone)", y = "Total Credit on Page") +
      xlim(0, input$maxtime[2])
  })
  # histogram of total scores faceted by version
  # bins = nrow(distinct(summary_data() , score))
  output$hist_total_version <- renderPlot({
    summary_data_versions() %>%
      group_by(userId, version_num, pageNumber) %>%
      summarize(total = max(pageCreditAchieved, na.rm = TRUE)) %>%
      ggplot(aes(x = total)) +
      geom_histogram() +
      labs(x = "Total Points", y = "Number of Students", title = "Total Scores on Assignment (Columns are version, rows are page number") +
      facet_grid(pageNumber ~ version_num)
  })
  
  #====================TIME TO QUESTION PLOTS===================
  #Average time per question
  output$time_to_question_av <- renderPlot({
    cleaned() %>%
      mutate(answer_num = coalesce(answerAncestor, componentName)) %>%
      group_by(userId) %>%
      mutate(time_dif = coalesce(as.numeric(c(NA, diff(
        time
      ))), as.numeric(time))) %>%
      group_by(userId, pageNumber, answer_num) %>%
      summarise(time = sum(time_dif)) %>%
      ungroup() %>%
      ggplot(aes(y = time, x = answer_num)) +
      geom_bar(stat = "summary", fun = "mean") +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_wrap( ~ pageNumber) +
      labs(x = "Question", y = "Time", title = "Average Time per Question")
  })
  #Accumolative time per question by userId
  output$time_to_question <- renderPlot({
    cleaned() %>%
      mutate(answer_num = coalesce(answerAncestor, componentName)) %>%
      group_by(userId) %>%
      mutate(time_dif = coalesce(as.numeric(c(NA, diff(
        time
      ))), as.numeric(time))) %>%
      group_by(userId, pageNumber, answer_num) %>%
      summarise(time = sum(time_dif)) %>%
      ungroup() %>%
      ggplot(aes(
        y = time,
        x = answer_num,
        group = userId,
        color = userId
      )) +
      geom_step() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_wrap( ~ pageNumber) +
      labs(x = "Question", y = "Time", title = "Time to Question")
  })
  
  # ================RADAR GRAPH=======================================
  
  output$radar_graph <- renderPlot({
    summary_data() %>%
      select(userId, item, pageNumber, itemCreditAchieved) %>%
      filter(!is.na(item)) %>%
      group_by(userId, pageNumber, item) %>%
      slice_max(itemCreditAchieved, n = 1) %>%
      distinct() %>%
      pivot_wider(names_from = c(item, pageNumber),
                  values_from = itemCreditAchieved) %>%
      ggradar()
  })
})

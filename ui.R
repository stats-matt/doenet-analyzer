# enableBookmarking(store = "url")

shinyUI(fluidPage(
  titlePanel("Doenet Data Analyzer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      actionButton("update", "Update Data", icon = icon("sync")),
      # bookmarkButton(),
      downloadButton("downloadData", "Download Data"),
      # h1("Compare experiments:"),
      # textInput("extra_id", "Extra DoenetID"),
      # actionButton("submit_extra", "Submit"),
      numericInput(
        "numid",
        "Number of Doenet IDs",
        value = 1,
        min = 1,
        max = 5,
        step = 1
      ),
      # actionButton("gennum","next"),
      
      # hard-coded ui for doenet ids
      uiOutput("rid"),
      uiOutput("id1"),
      uiOutput("id2"),
      uiOutput("id3"),
      uiOutput("id4"),
      uiOutput("id5"),
      actionButton("submit_extra", "Submit"),
      # slider
      numericInput("maxtime_set", "Slider maximum time:", 80000),
      uiOutput("time_slider"),
      uiOutput("date_slider"),
      uiOutput("version_select")
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Histogram by Problem", plotOutput("hist_prob")),
        tabPanel(
          "Submissions by Problem",
          tabsetPanel(
            tabPanel("Submissions Per Problem", 
                     "This graph shows the average number of submissions per question. Averages are calculated across all attempts and versions of the assignment.",
                     plotOutput("hist_submissions")),
            tabPanel(
              "Submissions vs Attempts and Versions",
              "This set of graphs shows the number of submissions per question, stratified by version number and attempt number. You may choose to display either cumulative submissions or average submissions per question.",
              fluidRow(
                selectInput(
                  "MeanVar",
                  "Display statistics by cumulative submissions or average submissions per student?",
                  c(
                    "Cumulative" = "cm",
                    "Mean" = "mean"
                  )
                ),
                column(12, plotOutput("hist_subm_attempt")),
                column(12, plotOutput("hist_subm_version"))
              )
            )
          )
        ),
        tabPanel("Histogram of Total Scores", plotOutput("hist_total")),
        tabPanel(
          "Question-Specific Data",
          "This set of graphs shows the distribution of submissions for a specific question, as well as the distribution of how many students solved or attempted the question. At the bottom of the page is a graph displaying student scores vs number of submissions.",
          fluidRow(
            textInput("subm_q", "Input the Name of a Question for Specific Data"),
            column(12, plotOutput("q_submissions")),
            column(12, plotOutput("q_pie")),
            column(12, plotOutput("score_dot"))
          )
        ),
        tabPanel("Time Plot", plotOutput("time_plot")),
        tabPanel("Time Plot from start", plotOutput("time_plot_s")),
        # numericInput("maxtime_set", "Slider maximum time:", 80000),
        # uiOutput("slider")
        # ),
        tabPanel(
          "Brief Summary",
          textOutput("num_students"),
          textOutput("num_pages"),
          textOutput("num_doenetIds"),
          textOutput("num_versions")
        ),
        tabPanel("Summary Data", dataTableOutput("summary")),
        tabPanel("Raw Data", dataTableOutput("raw")),
        tabPanel("Cleaned Data", dataTableOutput("cleaned_data_w_versions")),
        tabPanel("Wrong Answers", plotOutput("wrong_plot")),
        tabPanel(
          "Version Comparison",
          tabsetPanel(
            type = "tabs",
            tabPanel("Problem Averages", plotOutput("problem_avgs_version")),
            tabPanel("Time Plots by version", plotOutput("time_plot_version")),
            # numericInput("maxtime_set", "Slider maximum time:", 80000),
            # uiOutput("slider")
            # ),
            tabPanel(
              "Time Plots from start by version",
              plotOutput("time_plot_s_version")
            ),
            # numericInput("maxtime_set", "Slider maximum time:", 80000),
            # uiOutput("slider")
            # ),
            tabPanel(
              "Histogram of total scores by version",
              plotOutput("hist_total_version")
            )
          )
        ),
        tabPanel("Radar graph", plotOutput("radar_graph"))
        
      )
    )
  )
))

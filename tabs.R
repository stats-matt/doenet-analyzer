# tab for all content in doenetid ----
activity_tab <-
  tabPanel(
    "Analyze Activity/Content",
    "These tabs provide feedback on the entire activity",
    tabsetPanel(
      tabPanel(
        "Histogram of Total Scores",
        "These are the frequencies of the total scores achieved by all students",
        br(),
        br(),
        plotOutput("hist_total")
      ),
      tabPanel(
        "Time Plots from start by version",
        "These show the time until credit was achieved by students at different times from the time when the first student opened the activity",
        br(),
        br(),
        plotOutput("time_plot_s_version")
      ),
      tabPanel(
        "Time Plots by version",
        "These show the time until credit achieve from the time each student first opened their activity",
        br(),
        br(),
        plotOutput("time_plot_version")
      ),
      tabPanel(
        "Histogram of total scores by version",
        "This page shows a histogram of total score for each version of the activity",
        br(),
        br(),
        plotOutput("hist_total_version")
      )
    )
  )

# tab for analysis by problem ----
problem_tab <-
  tabPanel(
    "Analyze Individual Problems",
    "These tabs provide feedback at the problem level",
    tabsetPanel(
      tabPanel(
        "Version Comparison",
        tabsetPanel(
          type = "tabs",
          tabPanel("Problem Averages", plotOutput("problem_avgs_version"))
        )
      ),
      tabPanel(
        "Wrong Answers", 
        "This plot shows the most commonly entered wrong answer for each question",
        br(),
        br(),
        plotOutput("wrong_plot")
        ),
      tabPanel(
        "Radar graph", 
        "This is a radar graph of progress across all problems - note this is a work in progress",
        br(),
        br(),
        plotOutput("radar_graph")
        ),
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
      tabPanel("Histogram by Problem", plotOutput("hist_prob")),
      tabPanel(
        "Submissions Per Problem",
        "This graph shows the average number of submissions per question. Averages are calculated across all attempts and versions of the assignment.",
        plotOutput("hist_submissions")
      ),
      tabPanel(
        "Submissions vs Attempts and Versions",
        "This set of graphs shows the number of submissions per question, stratified by version number and attempt number. You may choose to display either cumulative submissions or average submissions per question.",
        fluidRow(
          selectInput(
            "MeanVar",
            "Display statistics by cumulative submissions or average submissions per student?",
            c("Cumulative" = "cm",
              "Mean" = "mean")
          ),
          column(12, plotOutput("hist_subm_attempt")),
          column(12, plotOutput("hist_subm_version"))
        )
      )
      
    )
  )

# tab for analysis by student ----
student_tab <-
  tabPanel(
    "Analyze Students",
    "These tabs provide feedback on students",
    tabsetPanel(
      tabPanel("Time Plot", plotOutput("time_plot")),
      tabPanel("Time Plot from start", plotOutput("time_plot_s"))
    )
  )
# numericInput("maxtime_set", "Slider maximum time:", 80000),
# uiOutput("slider")
# ),))

# tab for analyzing datasets ----
data_tab <-
  tabPanel(
    "View Data",
    "These tabs provide views of the data in various formats",
    tabsetPanel(
      tabPanel(
        "Brief Summary",
        textOutput("num_students"),
        textOutput("num_pages"),
        textOutput("num_doenetIds"),
        textOutput("num_versions")
      ),
      tabPanel("Summary Data", dataTableOutput("summary")),
      tabPanel("Raw Data", dataTableOutput("raw")),
      tabPanel("Cleaned Data", dataTableOutput("cleaned_data_w_versions"))
    )
  )

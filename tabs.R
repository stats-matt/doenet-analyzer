# tab for all content in doenetid ----
activity_tab <-
  tabPanel(
    "Analyze activity/content",
    "These tabs provide feedback on the entire activity",
    tabsetPanel(
      tabPanel(
        "Histogram of total scores",
        "Here are the frequencies of the total scores achieved by all students",
        br(),
        br(),
        plotOutput("hist_total")
      ),
      tabPanel(
        "Histogram of total scores by version",
        "This page shows a histogram of total score for each version of the activity",
        br(),
        br(),
        plotOutput("hist_total_version")
      ),
      tabPanel(
        "Time plot from first loading, by version",
        "Here are the times until credit was achieved, where time is computed from the time the activity was first loaded, by version.",
        br(),
        br(),
        plotOutput("time_plot_activity_version")
      ),
      tabPanel(
        "Time plots from each student loading, by version",
        "Here are the times until credit was achieved from the time each student first opened their activity",
        br(),
        br(),
        plotOutput("time_plot_person_version")
      )
      )
    )

# tab for analysis by problem ----
problem_tab <-
  tabPanel(
    "Analyze Individual Problems",
    "These tabs provide feedback at the problem level",
    tabsetPanel(
      tabPanel("Version Comparison",
               tabsetPanel(
                 type = "tabs",
                 tabPanel("Problem Averages", plotOutput("problem_avgs_version"))
               )),
      tabPanel(
        "All Answers",
        "This plot shows a representation of all submitted answers",
        br(),
        br(),
        plotOutput("all_answers_plot")
      ),
      tabPanel("Text tab",
               br(),
               br(),
               dataTableOutput("all_answers_text")),
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
      ),
      tabPanel("Time per Question",
               fluidRow(
                 column(12, plotOutput("time_to_question_av")),
                 column(12, plotOutput("time_to_question"))
               ))
    )
  )


# tab for analysis by student ----
# student_tab <-
#   tabPanel(
#     "Analyze Students",
#     "These tabs provide feedback on students",
#     tabsetPanel(
#       tabPanel("Time Plot", plotOutput("time_plot")),
#       tabPanel("Time Plot from start", plotOutput("time_plot_s"))
#     )
#   )

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
      tabPanel("Raw Data (events)", dataTableOutput("events")),
      tabPanel("Cleaned, all versions", dataTableOutput("cleaned_versions")),
      tabPanel("Summary, all versions", dataTableOutput("summary_data_versions")),
      tabPanel("Cleaned, selected version", dataTableOutput("cleaned")),
      tabPanel("Summary, selected version", dataTableOutput("summary_data"))
    )
  )

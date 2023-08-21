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
        h4("How to Use the Graph:"),
        p("The graph displays common answers that are shared among students. 
        An answer is considered \"common\" if its frequency is greater than ten. 
        Here's how you can utilize the graph:"),
        tags$ol(
          tags$li("To focus on a specific page and item combination, use the 
                  dropdown menu provided. Select the desired page and item, and 
                  the graph will adjust accordingly."),
          tags$li("The slider allows you to control the maximum frequency of 
                  answers displayed in the graph. Slide it to a lower value if 
                  you want to exclude highly common answers and focus on less 
                  frequent responses."),
          tags$li("Keep in mind that if a particular answer is significantly 
                  more common than others, adjusting the slider towards zero 
                  will cause that answer to disappear from the graph.")),
        br(),
        br(),
      fluidRow(
        column(6, selectInput("page_dropdown_all", "Select Page", "")),
        column(6, selectInput("item_dropdown_all", "Select Item", ""))
      ),
      sliderInput("integer_slider_all", "Most Frequent Answers",
                  min = 0,
                  max = 0,
                  value = 0),
      # # Render the plot
      plotlyOutput("all_answers_plot")),
      tabPanel("Text tab",
               br(),
               br(),
               DT::DTOutput("all_answers_text")),
      tabPanel(
        "Wrong Answers",
        h4("How to Use the Graph:"),
        p("The graph displays common answers that are shared among students. 
        An answer is considered \"common\" if its frequency is greater than ten. 
        Here's how you can utilize the graph:"),
        tags$ol(
          tags$li("To focus on a specific page and item combination, use the 
                  dropdown menu provided. Select the desired page and item, and 
                  the graph will adjust accordingly."),
          tags$li("The slider allows you to control the maximum frequency of 
                  answers displayed in the graph. Slide it to a lower value if 
                  you want to exclude highly common answers and focus on less 
                  frequent responses."),
          tags$li("Keep in mind that if a particular answer is significantly 
                  more common than others, adjusting the slider towards zero 
                  will cause that answer to disappear from the graph.")),
        br(),
        br(),
        fluidRow(
          column(6, selectInput("page_dropdown", "Select Page", "")),
          column(6, selectInput("item_dropdown", "Select Item", ""))
        ),
        sliderInput("slider_wrong", 
                    "Most Frequent Wrong Answers",
                    min = 0, 
                    max = 0,
                    value = 0),
        # Render the plot
        plotlyOutput("wrong_plot")),
      # tabPanel(
      #   "Radar graph",
      #   "This is a radar graph of progress across all problems - note this is a work in progress",
      #   br(),
      #   br(),
      #   plotOutput("radar_graph")
      # ),
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
          column(12, plotOutput("hist_subm_version")))),
      tabPanel(
        "Free Response Questions (FRQ)",
          h4("How Do I Interpret The Graphs?"),
          p("The subsequent graphs are employed to analyze \"unstructured\" text 
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
        sliderInput("wordcloud_int", 
                    "Class size",
                    min = 1, 
                    max = 1,
                    value = 1),
        selectInput("componentType_dropdown", 
                    "Select Type Of Question",
                    choices = c("Answer",
                                "Choice Input",
                                "Text Input")),
        tabsetPanel(
          tabPanel("Graph 1", plotOutput("graph1")),
          tabPanel("Graph 2", plotOutput("graph2")),
          tabPanel("Word Cloud", plotOutput("wordcloud"))
        )
      )
    )
      #,
      # tabPanel("Time per Question",
      #          fluidRow(
      #            column(12, plotOutput("time_to_question_av")),
      #            column(12, plotOutput("time_to_question"))
      #          ))
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
      tabPanel("Raw Data (events)", DT::DTOutput("events_dt")),
      tabPanel("Cleaned, all versions", DT::DTOutput("cleaned_versions_dt")),
      tabPanel("Summary, all versions", DT::DTOutput("summary_data_versions_dt")),
      tabPanel("Cleaned, selected version", DT::DTOutput("cleaned_dt")),
      tabPanel("Summary, selected version", DT::dataTableOutput("summary_data_dt"))
    )
  )

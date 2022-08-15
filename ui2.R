#enableBookmarking(store = "url")

shinyUI(fluidPage(
  titlePanel("Doenet Data Analyzer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      actionButton("update", "Update Data", icon = icon("sync")),
      #bookmarkButton(),
      downloadButton('downloadData', 'Download Data'),
      #h1("Compare experiments:"),
      #textInput("extra_id", "Extra DoenetID"),
      #actionButton("submit_extra", "Submit"),
      numericInput(
        "numid",
        "Number of Doenet IDs",
        value = 1,
        min = 1,
        max = 5,
        step = 1
      ),
      #actionButton("gennum","next"),
      
      #hard-coded ui for doenet ids
      uiOutput("rid"),
      uiOutput("id1"),
      uiOutput("id2"),
      uiOutput("id3"),
      uiOutput("id4"),
      uiOutput("id5"),
      actionButton("submit_extra", "Submit"),
      uiOutput("date_slider"),
      uiOutput("version_select")
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Histogram by Problem", plotOutput("hist_prob")),
        tabPanel("Submissions by Problem",
                 tabsetPanel(
                   tabPanel("Submissions Per Problem",
                            fluidRow(
                              column(12, plotOutput("hist_submissions")),
                              textInput("subm_q", "Input the Name of a Question for Specific Data"),
                              column(12, plotOutput("q_submissions"))
                            )
                   ),
                   tabPanel("Submissions vs Attempts",plotOutput("hist_subm_attempt")))),
        tabPanel("Histogram of Total Scores", plotOutput("hist_total")),
        tabPanel("Time Plot", 
                 numericInput("maxtime_set_normal", "Slider maximum time:", 80000),
                 uiOutput("time_slider_normal"),
                 plotOutput("time_plot"),),  
        tabPanel(
          "Brief Summary",
          textOutput("num_students"),
          textOutput("num_pages"),
          textOutput("num_doenetIds"),
          textOutput("num_versions")
        ),
        tabPanel("Summary Data", dataTableOutput("summary")),
        tabPanel("Raw Data", dataTableOutput("raw")),
        tabPanel("Cleaned Data", dataTableOutput("cleaned_data")),
        tabPanel("Wrong Answers", plotOutput("wrong_plot")),

        tabPanel("Version Comparison", tabsetPanel(type = "tabs",
                                                   tabPanel("Problem Averages", plotOutput("problem_avgs_version")),
                                                   tabPanel("Time Plots by version", 
                                                            numericInput("maxtime_set_version", "Slider maximum time:", 80000),
                                                            uiOutput("time_slider_version"),
                                                            plotOutput("time_plot_version")),
                                                   tabPanel("Histogram of total scores by version", plotOutput("hist_total_version"))
                                                            
                                                   
                 
        )
        
      )
    )
  )
)))

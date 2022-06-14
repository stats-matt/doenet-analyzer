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
      numericInput("numid","Number of Doenet IDs",value=1, min = 1, max = 5, step=1),
      #actionButton("gennum","next"),
      
      #hard-coded ui for doenet ids
      uiOutput("rid"),
      uiOutput("id1"),
      uiOutput("id2"),
      uiOutput("id3"),
      uiOutput("id4"),
      uiOutput("id5"),
      actionButton("submit_extra", "Submit"),
      #slider
      numericInput("maxtime_set", "Slider maximum time:", 80000),
      uiOutput("slider")
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        
        tabPanel("Histogram by Problem", plotOutput("hist_prob")),
        tabPanel("Submissions by Problem", plotOutput("hist_submissions")),
        tabPanel("Histogram of Total Scores", plotOutput("hist_total")),
        tabPanel("Time Plot", plotOutput("time_plot")),
        tabPanel("Time Plot from start", plotOutput("time_plot_s")),
        tabPanel(
          "Brief Summary",
          textOutput("num_students"),
          textOutput("num_pages"),
          textOutput("num_doenetIds")
        ),
        tabPanel("Summary Data", dataTableOutput("summary")),
        tabPanel("Raw Data", dataTableOutput("raw")),
        tabPanel("Cleaned Data", dataTableOutput("cleaned_data")),
        tabPanel("version-version", plotOutput("version_version"))
        
      )
    )
  )
))

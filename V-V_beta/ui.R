shinyUI(fluidPage(
  titlePanel("Version-Version comparison demonstrator"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      actionButton("update", "Update Data", icon = icon("sync")),

      downloadButton('downloadData', 'Download Data'),

      
     
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Raw Data", dataTableOutput("raw")),
        tabPanel("summarized data", dataTableOutput("summarized_data")),
        tabPanel("Histogram by Problem", plotOutput("hist_prob")),
        tabPanel("Cleaned Data", dataTableOutput("cleaned_data")),
        tabPanel("Comparing Version Difficulties",plotOutput("graph3")),
        tabPanel("Comparing questions V-V", plotOutput("graph4"))
        )
        
      )
    )
  )
)
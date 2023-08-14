# enableBookmarking(store = "url")

source("tabs.R")

shinyUI(fluidPage(
  titlePanel("Doenet Data Analyzer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      actionButton("update", "Update Data", icon = icon("sync")), # probably not needed
      downloadButton("downloadData", "Download Data"), # maybe not needed?? or move to data tab?
      selectInput("activity_select",
                  "Select an activity:",
                  choices = NULL),
      checkboxGroupInput(
        "outlier_check",
        label = "Select Options:",
        choices = c("Include Outlier", "Exclude Outlier"),
        selected = FALSE
      ),
      conditionalPanel(condition = "output.show_pulldown",
                       selectInput(
                         "select",
                         label = h3("Select box"),
                         choices = c()
                       )),
      # slider
      numericInput("maxtime_set", "Slider maximum time:", 80000),
      uiOutput("time_slider"),
      uiOutput("date_slider"),
      uiOutput("version_select")
    ),
    mainPanel(
      tabsetPanel(type = "pills",
                  activity_tab,
                  problem_tab,
                  data_tab)
    )
  )
))

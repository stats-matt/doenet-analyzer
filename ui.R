# enableBookmarking(store = "url")

# library(shiny)
# library(tidyverse)
# library(jsonlite)
# library(anytime)
# library(dplyr)
# library(scales)
# library(DT)
# 
# # devtools::install_github("ricardo-bion/ggradar")
# library(ggradar)

source("tabs.R")

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
      selectInput("dropdown", "Select an option:", choices = NULL),
      actionButton("submit_extra", "Submit"),
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
                  #student_tab,
                  data_tab)
    )
  )
))

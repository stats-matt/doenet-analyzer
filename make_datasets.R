# Input from date slider determines which dates are included in the set.
# cleaned_versions <- reactive({
#   #req(events())
#   withProgress(message = 'Analyzing Data', {
#     clean_events(events(), input$time_slider[1], input$time_slider[2])
#     #clean_events(events())
#   })
# })

cleaned_versions <- reactive({
  #clean_events(events(), input$time_slider[1], input$time_slider[2])
  clean_events(events())
})

summary_data_versions <- reactive({
  summarize_events(cleaned_versions())
})

# Filter takes in previously cleaned data and then the version we select
cleaned <- reactive({
  version_filter(cleaned_versions(), input$version_select)
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


# observe({
#   updateSelectInput(
#     session = session,
#     inputId = "version_select",
#     choices = c(1:versions())
#     #choices = c("All", 1:versions()),
#   )
# })

# Slider for time in the time plots
# output$time_slider <-
#   renderUI({
#     sliderInput(
#       "maxtime",
#       min = 0,
#       "Maximum time shown:",
#       max = input$maxtime_set,
#       value =  c(500, 10000)
#     )
#   })

# Slider for date
# output$date_slider <-
#   renderUI({
#     sliderInput(
#       "date_range",
#       "Data from: ",
#       min = min(dates()),
#       max = max(dates()),
#       value = c(min(dates()),
#                 max(dates()))
#     )
#   })

# events <- reactive({
#   load_data(query())})

# output$show_pulldown <- reactive({
#   length(unlist(query())) > 1
# })
# outputOptions(output, "show_pulldown", suspendWhenHidden = FALSE)
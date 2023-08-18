# =========================DATA TABLES===========================================
# creates tables of each of the data versions to view/troubleshoot in a tab
output$events_dt <-
  DT::renderDT(events())
output$cleaned_versions_dt <-
  DT::renderDT(cleaned_versions())
output$summary_data_versions_dt <-
  DT::renderDT(summary_data_versions())
output$cleaned_dt <-
  DT::renderDT(cleaned())
output$summary_data_dt <-
  DT::renderDT(summary_data())

# =======================SUMMARY TEXT============================================
# creates an output text detailing how many students in the data set
output$num_students <-
  renderText(paste0(
    "There is/are ",
    n_distinct(events()$userId, na.rm = TRUE),
    " student(s)"
  ))
# creates an output text detailing how many versions are present in the set
output$num_versions <-
  renderText(paste0("There are ", versions()))

# creates an output text detailing how many different doenet experiments
# are represented in this set.
output$num_doenetIds <-
  renderText(paste0(
    "There is/are ",
    n_distinct(events()$doenetId, na.rm = TRUE),
    " doenetId(s)"
  ))
# creates an output text detailing how many pages are included in this dataset
output$num_pages <-
  renderText(paste0(
    "There is/are ",
    n_distinct(summary_data_versions()$pageNumber, na.rm = TRUE),
    " page(s)"
  ))
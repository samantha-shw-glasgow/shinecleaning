# Currently, this only tests that an Excel file is created that can be read again
# without erroring. As a byproduct, an output file is created in tests/testthat,
# which is helpful for development purposes right now.
test_that("a valid Excel spreadsheet is created", {
  data <- tibble::tribble(
    ~"Error messages",              ~"Include?", ~"Reviewer notes", ~id, ~dob,
    "",                             TRUE,        "", 1, lubridate::ymd("2010-01-01"),
    "",                             TRUE,        "", 2, lubridate::ymd("2010-01-02"),
    "Duplicate DOB; Duplicate DOB", TRUE,        "", 3, lubridate::ymd("2010-01-03"),
    "Duplicate DOB; Duplicate DOB", TRUE,        "", 4, lubridate::ymd("2010-01-03"),
    "",                             TRUE,        "", 5, lubridate::ymd("2010-01-04"),
  )
  output <- create_spreadsheet(data, "cleaning_spreadsheet.xlsx")
  openxlsx::read.xlsx("cleaning_spreadsheet.xlsx", 1)
})

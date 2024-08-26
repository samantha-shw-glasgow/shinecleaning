# Currently, this only tests that an Excel file is created that can be read again
# without erroring. As a byproduct, an output file is created in tests/testthat,
# which is helpful for development purposes right now.
test_that("a valid Excel spreadsheet is created", {
  data <- tibble::tribble(
    ~"Error messages",              ~"Keep row?", ~"Reviewer notes", ~id, ~dob,
    "",                             1,        "", 1, lubridate::ymd("2010-01-01"),
    "",                             1,        "", 2, lubridate::ymd("2010-01-02"),
    "Duplicate DOB; Duplicate DOB", 1,        "", 3, lubridate::ymd("2010-01-03"),
    "Duplicate DOB; Duplicate DOB", 1,        "", 4, lubridate::ymd("2010-01-03"),
    "",                             1,        "", 5, lubridate::ymd("2010-01-04"),
  )
  output <- create_spreadsheet(data, file.path(tempdir(), "cleaning_spreadsheet.xlsx"))
  expect_no_error(openxlsx::read.xlsx(file.path(tempdir(), "cleaning_spreadsheet.xlsx"), 1))
})

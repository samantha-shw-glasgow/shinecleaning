# Currently, this only tests that an Excel file is created that can be read again
# without erroring. As a byproduct, an output file is created, which can be
# helpful for manual testing.
test_that("a valid Excel spreadsheet is created", {
  data <- SHINEcleaning::pri_valid_responses |>
    apply_cleaning_rules(list(partial_cases, duplicate_cases, age_year_mismatch)) |>
    dplyr::mutate(age = calculate_age(RecordedDate, dobyr, dobmnth, dobday))

  output <- create_spreadsheet(data, file.path(tempdir(), "cleaning_spreadsheet.xlsx"))
  expect_no_error(openxlsx::read.xlsx(file.path(tempdir(), "cleaning_spreadsheet.xlsx"), 1))

  # # Uncomment this line to create a spreadsheet in tests/testthat instead of a temp directory
  # create_spreadsheet(data, file.path(".", "cleaning_spreadsheet.xlsx"))
})

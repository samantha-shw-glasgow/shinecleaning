test_that("columns are added with a single validator", {
  validators <- list(check_duplicate_dob)
  input <- tibble::tribble(
    ~id, ~dob,
    1, lubridate::ymd("2010-01-01"),
    2, lubridate::ymd("2010-01-02"),
    3, lubridate::ymd("2010-01-03"),
    4, lubridate::ymd("2010-01-03"),
    5, lubridate::ymd("2010-01-04"),
  )
  expected <- tibble::tribble(
    ~"Error messages", ~"Include?", ~"Reviewer notes", ~id, ~dob,
    "",                TRUE,        "", 1, lubridate::ymd("2010-01-01"),
    "",                TRUE,        "", 2, lubridate::ymd("2010-01-02"),
    "Duplicate DOB",   TRUE,        "", 3, lubridate::ymd("2010-01-03"),
    "Duplicate DOB",   TRUE,        "", 4, lubridate::ymd("2010-01-03"),
    "",                TRUE,        "", 5, lubridate::ymd("2010-01-04"),
  )
  result <- run_validations(input, validators)
  expect_identical(result, expected)
})

test_that("columns are added with multiple validators", {
  validators <- list(check_duplicate_dob, check_duplicate_dob)
  input <- tibble::tribble(
    ~id, ~dob,
    1, lubridate::ymd("2010-01-01"),
    2, lubridate::ymd("2010-01-02"),
    3, lubridate::ymd("2010-01-03"),
    4, lubridate::ymd("2010-01-03"),
    5, lubridate::ymd("2010-01-04"),
  )
  expected <- tibble::tribble(
    ~"Error messages",              ~"Include?", ~"Reviewer notes", ~id, ~dob,
    "",                             TRUE,        "", 1, lubridate::ymd("2010-01-01"),
    "",                             TRUE,        "", 2, lubridate::ymd("2010-01-02"),
    "Duplicate DOB; Duplicate DOB", TRUE,        "", 3, lubridate::ymd("2010-01-03"),
    "Duplicate DOB; Duplicate DOB", TRUE,        "", 4, lubridate::ymd("2010-01-03"),
    "",                             TRUE,        "", 5, lubridate::ymd("2010-01-04"),
  )
  result <- run_validations(input, validators)
  expect_identical(result, expected)
})

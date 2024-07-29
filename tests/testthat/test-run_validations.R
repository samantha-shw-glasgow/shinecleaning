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
    ~"Error messages", ~"Keep row?", ~"Reviewer notes", ~id, ~dob,
    "",                1,            "", 1, lubridate::ymd("2010-01-01"),
    "",                1,            "", 2, lubridate::ymd("2010-01-02"),
    "Duplicate DOB",   1,            "", 3, lubridate::ymd("2010-01-03"),
    "Duplicate DOB",   1,            "", 4, lubridate::ymd("2010-01-03"),
    "",                1,            "", 5, lubridate::ymd("2010-01-04"),
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
    ~"Error messages",              ~"Keep row?", ~"Reviewer notes", ~id, ~dob,
    "",                             1,            "", 1, lubridate::ymd("2010-01-01"),
    "",                             1,            "", 2, lubridate::ymd("2010-01-02"),
    "Duplicate DOB; Duplicate DOB", 1,            "", 3, lubridate::ymd("2010-01-03"),
    "Duplicate DOB; Duplicate DOB", 1,            "", 4, lubridate::ymd("2010-01-03"),
    "",                             1,            "", 5, lubridate::ymd("2010-01-04"),
  )
  result <- run_validations(input, validators)
  expect_identical(result, expected)
})

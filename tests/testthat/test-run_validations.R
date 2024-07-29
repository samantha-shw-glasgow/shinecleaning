test_that("validation column is added", {
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
    ~id, ~dob, ~errors,
    1, lubridate::ymd("2010-01-01"), "",
    2, lubridate::ymd("2010-01-02"), "",
    3, lubridate::ymd("2010-01-03"), "Duplicate DOB",
    4, lubridate::ymd("2010-01-03"), "Duplicate DOB",
    5, lubridate::ymd("2010-01-04"), "",
  )
  result <- run_validations(input, validators)
  expect_identical(result, expected)
})

test_that("multiple validators work", {
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
    ~id, ~dob, ~errors,
    1, lubridate::ymd("2010-01-01"), "",
    2, lubridate::ymd("2010-01-02"), "",
    3, lubridate::ymd("2010-01-03"), "Duplicate DOB; Duplicate DOB",
    4, lubridate::ymd("2010-01-03"), "Duplicate DOB; Duplicate DOB",
    5, lubridate::ymd("2010-01-04"), "",
  )
  result <- run_validations(input, validators)
  expect_identical(result, expected)
})

test_that("validation column is added", {
  validators <- list(check_duplicate_dob)
  input <- data.frame(
    id = 1:5,
    dob = c(
      lubridate::ymd("2010-01-01"),
      lubridate::ymd("2010-01-02"),
      lubridate::ymd("2010-01-03"),
      lubridate::ymd("2010-01-03"),
      lubridate::ymd("2010-01-04")
    )
  )
  expected <- input
  expected$errors <- c(NA, NA, "Duplicate DOB", "Duplicate DOB", NA)
  result <- run_validations(input, validators)
  expect_identical(result, expected)
})

test_that("multiple validators work", {
  validators <- list(check_duplicate_dob, check_duplicate_dob)
  input <- data.frame(
    id = 1:5,
    dob = c(
      lubridate::ymd("2010-01-01"),
      lubridate::ymd("2010-01-02"),
      lubridate::ymd("2010-01-03"),
      lubridate::ymd("2010-01-03"),
      lubridate::ymd("2010-01-04")
    )
  )
  expected <- input
  expected$errors <- c(
    NA,
    NA,
    "Duplicate DOB; Duplicate DOB",
    "Duplicate DOB; Duplicate DOB",
    NA
  )
  result <- run_validations(input, validators)
  expect_identical(result, expected)
})

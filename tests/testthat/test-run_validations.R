test_that("validation column is added", {
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
  expected$errors <- c(NA, NA, "Duplicate", "Duplicate", NA)
  expect_identical(run_validations(input), expected)
})

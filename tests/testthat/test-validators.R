test_validator_with_data <- function(validator_fun, filename) {
  data <- readr::read_csv(
    file.path("examples", filename),
    na = character(),
    show_col_types = FALSE
  )
  result <- validator_fun(data)
  expect_equal(result$include, data$expected_include)
  expect_equal(result$message, data$expected_message)
}

test_that("duration_too_short", {
  test_validator_with_data(duration_too_short, "duration_too_short.csv")
})

test_that("no_test_responses", {
  test_validator_with_data(no_test_responses, "no_test_responses.csv")
})

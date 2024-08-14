test_that("columns are added with a single validator", {
  validators <- list(no_test_responses)
  input <- tibble::tribble(
    ~id, ~Status,
    1, "IP Address",
    2, "Survey Preview",
  )
  expected <- tibble::tribble(
    ~"Error messages", ~"Keep row?", ~"Reviewer notes", ~id, ~Status,
    "",                 1, "", 1, "IP Address",
    "Preview response", 0, "", 2, "Survey Preview",
  )
  result <- apply_cleaning_rules(input, validators)
  expect_identical(result, expected)
})

test_that("columns are added with multiple validators", {
  validators <- list(no_test_responses, duration_too_short)
  input <- tibble::tribble(
    ~id, ~Status, ~"Duration (in seconds)",
    1, "IP Address",     60,
    2, "IP Address",     59,
    3, "Survey Preview", 62,
    4, "Survey Preview", 58,
  )
  expected <- tibble::tribble(
    ~"Error messages", ~"Keep row?", ~"Reviewer notes", ~id, ~Status, ~"Duration (in seconds)",
    "",                                     1, "", 1, "IP Address",     60,
    "Duration too short",                   1, "", 2, "IP Address",     59,
    "Preview response",                     0, "", 3, "Survey Preview", 62,
    "Preview response; Duration too short", 0, "", 4, "Survey Preview", 58,
  )
  result <- apply_cleaning_rules(input, validators)
  expect_identical(result, expected)
})

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

describe("duration_too_short", {
  it("works as described in the example dataset", {
    test_validator_with_data(duration_too_short, "duration_too_short.csv")
  })
})

describe("no_test_responses", {
  it("works as described in the example dataset", {
    test_validator_with_data(no_test_responses, "no_test_responses.csv")
  })
})

describe("duplicate_cases", {
  it("works as described in the example dataset", {
    test_validator_with_data(duplicate_cases, "duplicate_cases.csv")
  })
})

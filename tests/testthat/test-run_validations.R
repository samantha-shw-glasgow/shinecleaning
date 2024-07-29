test_that("columns are added with a single validator", {
  validators <- list(duration_too_short)
  input <- tibble::tribble(
    ~id, ~"Duration (in seconds)",
    1, 60,
    2, 100,
    3, 50,
    4, 59,
  )
  expected <- tibble::tribble(
    ~"Error messages", ~"Keep row?", ~"Reviewer notes", ~id, ~"Duration (in seconds)",
    "",                   1, "", 1, 60,
    "",                   1, "", 2, 100,
    "Duration too short", 0, "", 3, 50,
    "Duration too short", 0, "", 4, 59,
  )
  result <- run_validations(input, validators)
  expect_identical(result, expected)
})

test_that("columns are added with multiple validators", {
  validators <- list(duration_too_short, duration_too_short)
  input <- tibble::tribble(
    ~id, ~"Duration (in seconds)",
    1, 60,
    2, 100,
    3, 50,
    4, 59,
  )
  expected <- tibble::tribble(
    ~"Error messages", ~"Keep row?", ~"Reviewer notes", ~id, ~"Duration (in seconds)",
    "",                                       1, "", 1, 60,
    "",                                       1, "", 2, 100,
    "Duration too short; Duration too short", 0, "", 3, 50,
    "Duration too short; Duration too short", 0, "", 4, 59,
  )
  result <- run_validations(input, validators)
  expect_identical(result, expected)
})

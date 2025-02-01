test_that("columns are added with a single validator", {
  validators <- list(no_test_responses)
  input <- tibble::tribble(
    ~id, ~Status,
    1, "IP Address",
    2, "Survey Preview",
  )
  expected <- tibble::tribble(
    ~"Error messages", ~"Keep row?", ~"Reviewer notes", ~id, ~Status,
    "", 1, "", 1, "IP Address",
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
    show_col_types = FALSE
  ) |>
    dplyr::mutate(
      expected_message = ifelse(is.na(expected_message), "", expected_message)
    )
  result <- validator_fun(data)
  expect_equal(result$include, data$expected_include)
  expect_equal(result$message, data$expected_message)

  # Test with the rows in the input data rearranged in arbitrary order.
  # We do this 10 times with different seeds.
  # Only a single failure message is shown to avoid cluttered test results.
  differences <- 0
  for (i in 1:10) {
    set.seed(i)
    data <- dplyr::slice(data, sample(1:dplyr::n()))
    result <- validator_fun(data)
    differences <- differences +
      length(waldo::compare(result$include, data$expected_include)) +
      length(waldo::compare(result$message, data$expected_message))
  }
  expect(differences == 0, "Test fails when input data are reordered")
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

describe("partial_cases", {
  it("works as described in the example dataset (primary)", {
    test_validator_with_data(partial_cases, "partial_cases_primary.csv")
  })

  it("works as described in the example dataset (primary)", {
    test_validator_with_data(partial_cases, "partial_cases_secondary.csv")
  })
})

describe("duplicate_cases", {
  it("works as described in the example dataset", {
    test_validator_with_data(duplicate_cases, "duplicate_cases.csv")
  })
})

describe("suggest_missing_class", {
  it("works as described in the example dataset", {
    test_validator_with_data(suggest_missing_class, "suggest_missing_class.csv")
  })
})

describe("recurring_postcodes", {
  it("works as described in the example dataset", {
    test_validator_with_data(recurring_postcodes, "recurring_postcodes.csv")
  })
})

describe("no_consent", {
  it("works as described in the example dataset", {
    test_validator_with_data(no_consent, "no_consent.csv")
  })
})

describe("age_year_mismatch", {
  it("works as described in the example dataset", {
    test_validator_with_data(age_year_mismatch, "age_year_mismatch.csv")
  })

  it("parses both date formats", {
    test_validator_with_data(age_year_mismatch, "age_year_mixed_dates.csv")
  })
})

describe("straightlining", {
  it("works as described in the example dataset (primary)", {
    test_validator_with_data(straightlining, "straightlining_primary.csv")
  })

  it("works as described in the example dataset (primary)", {
    test_validator_with_data(straightlining, "straightlining_secondary.csv")
  })
})

describe("missing School ID", {
  it("detects missing ID", {
    test_validator_with_data(has_school_id, "missing_school_id.csv")
  })
})

describe("Parsing DOBs", {
  it("detects invalid DOBs", {
    test_validator_with_data(valid_dob, "valid_invalid_dobs.csv")
  })
})

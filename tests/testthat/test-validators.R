describe("duration_too_short", {
  it("succeeds when duration is 61", {
    input <- tibble::tibble("Duration (in seconds)" = 61)
    expected <- tibble::tibble(include = TRUE, message = "")
    expect_equal(duration_too_short(input), expected)
  })

  it("succeeds when duration is exactly 60", {
    input <- tibble::tibble("Duration (in seconds)" = 60)
    expected <- tibble::tibble(include = TRUE, message = "")
    expect_equal(duration_too_short(input), expected)
  })

  it("fails when duration is 59", {
    input <- tibble::tibble("Duration (in seconds)" = 59)
    expected <- tibble::tibble(include = TRUE, message = "Duration too short")
    expect_equal(duration_too_short(input), expected)
  })

  it("handles multiple rows correctly", {
    input <- tibble::tibble("Duration (in seconds)" = c(50, 59, 60, 61))
    expected <- tibble::tibble(
      include = c(TRUE, TRUE, TRUE, TRUE),
      message = c("Duration too short", "Duration too short", "", "")
    )
    expect_equal(duration_too_short(input), expected)
  })
})

describe("no_test_responses", {
  it("succeeds when Status is IP Address", {
    input <- tibble::tibble("Status" = "IP Address")
    expected <- tibble::tibble(include = TRUE, message = "")
    expect_equal(no_test_responses(input), expected)
  })

  it("fails when Status is Survey Preview", {
    input <- tibble::tibble("Status" = "Survey Preview")
    expected <- tibble::tibble(include = FALSE, message = "Preview response")
    expect_equal(no_test_responses(input), expected)
  })

  it("fails when Status is something nonsensical", {
    input <- tibble::tibble("Status" = "something nonsensical")
    expected <- tibble::tibble(include = FALSE, message = "Unexpected response type")
    expect_equal(no_test_responses(input), expected)
  })

  it("handles multiple rows correctly", {
    input <- tibble::tibble("Status" = c("IP Address", "Survey Preview", "bloop"))
    expected <- tibble::tibble(
      include = c(TRUE, FALSE, FALSE),
      message = c("", "Preview response", "Unexpected response type")
    )
    expect_equal(no_test_responses(input), expected)
  })
})

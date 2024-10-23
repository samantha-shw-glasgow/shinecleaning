describe("censor_summary_data", {
  it("adds a censored column", {
    input <- tibble::tribble(
      ~description, ~denominator, ~numerator,
      "Description", 1, 0,
      "Description", 1, 1,
      "Description", 2, 0,
      "Description", 2, 1,
      "Description", 2, 2,
      "Description", 3, 0,
      "Description", 3, 1,
      "Description", 3, 2,
      "Description", 3, 3,
      "Description", 4, 0,
      "Description", 4, 1,
      "Description", 4, 2,
      "Description", 4, 3,
      "Description", 4, 4,
      "Description", 5, 0,
      "Description", 5, 1,
      "Description", 5, 2,
      "Description", 5, 3,
      "Description", 5, 4,
      "Description", 5, 5,
    )
    expected <- tibble::tribble(
      ~description, ~denominator, ~numerator, ~censored,
      "Description", 1, 0, TRUE,
      "Description", 1, 1, TRUE,
      "Description", 2, 0, TRUE,
      "Description", 2, 1, TRUE,
      "Description", 2, 2, TRUE,
      "Description", 3, 0, TRUE,
      "Description", 3, 1, TRUE,
      "Description", 3, 2, TRUE,
      "Description", 3, 3, FALSE,
      "Description", 4, 0, TRUE,
      "Description", 4, 1, TRUE,
      "Description", 4, 2, TRUE,
      "Description", 4, 3, FALSE,
      "Description", 4, 4, FALSE,
      "Description", 5, 0, TRUE,
      "Description", 5, 1, TRUE,
      "Description", 5, 2, TRUE,
      "Description", 5, 3, FALSE,
      "Description", 5, 4, FALSE,
      "Description", 5, 5, FALSE,
    )
    result <- censor_summary_data(input, "numerator")
    expect_identical(result, expected)
  })
})

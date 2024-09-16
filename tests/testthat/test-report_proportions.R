input_data <- function() {
  tibble::tribble(
    ~gender, ~class,     ~health,
    "Girls",   "S6",      "Good",
    "Boys",   "S6",      "Fair",
    "Girls",   "S6",      "Good",
    "Girls",   "S6", "Excellent",
    "Girls",   "S6",      "Poor",
    "Boys",   "S6",      "Poor",
    "Boys",   "S6",      "Good",
    "Girls",   "S6",      "Fair",
    "Girls",   "S1",      "Good",
    "Boys",   "S1",      "Good",
    "Girls",   "S1",      "Fair",
    "Girls",   "S1",      "Poor",
    "Boys",   "S1",      "Poor",
    "Girls",   "S1",      "Poor",
    "Girls",   "S1",      "Poor",
    "Boys",   "S1", "Excellent",
    "Girls",   "S1",      "Poor",
    "Girls",   "S1", "Excellent",
    "Boys",   "S1",      "Fair",
    "Boys",   "S1",      "Good"
  )
}

classes <- c("S1", "S2", "S3", "S4", "S5", "S6")
genders <- c("Boys", "Girls", "All")

describe("collapsed summary", {
  it("works without censoring and without gender split", {
    expected <- tibble::tribble(
      ~class, ~gender,  ~numerator, ~denom,
      # "S1",   "All",    5,          12,
      # "S6",   "All",    4,          8,
      "All",  "All pupils",    9,          20,
    ) |>
      mutate(class = forcats::fct_inorder(class))
    result <- create_collapsed_summary(
      input_data(),
      health,
      c("Excellent", "Good"),
      genders, classes, .gender_split = FALSE
    )
    expect_equal(result, expected)
  })

  it("works without censoring and with gender split", {
    expected <- tibble::tribble(
      ~class, ~gender,  ~numerator, ~denom,
      "S1",   "Boys",   3,          5,
      "S1",   "Girls",  2,          7,
      "S6",   "Boys",   1,          3,
      "S6",   "Girls",  3,          5,
      "All",  "All",    9,          20,
    ) |>
      mutate(class = forcats::fct_inorder(class))
    result <- create_collapsed_summary(
      input_data(),
      health,
      c("Excellent", "Good"),
      genders, classes,
      .gender_split = TRUE
    )
    expect_equal(result, expected)
  })
})

describe("full summary", {
  it("works without censoring and without gender split", {
    expected <- tibble::tribble(
      ~class, ~gender,  ~answer,      ~numerator, ~denom,
      # "S1",   "All",    "Excellent",  2,          12,
      # "S1",   "All",    "Fair",       2,          12,
      # "S1",   "All",    "Good",       3,          12,
      # "S1",   "All",    "Poor",       5,          12,
      # "S6",   "All",    "Excellent",  1,          8,
      # "S6",   "All",    "Fair",       2,          8,
      # "S6",   "All",    "Good",       3,          8,
      # "S6",   "All",    "Poor",       2,          8,
      "All",  "All",    "Excellent",  3,          20,
      "All",  "All",    "Good",       6,          20,
      "All",  "All",    "Fair",       4,          20,
      "All",  "All",    "Poor",       7,          20,
    ) |>
      mutate(class = forcats::fct_inorder(class),
        answer = factor(answer, levels = c("Poor", "Fair", "Good", "Excellent")))
    result <-
      create_full_summary(input_data(),
        health,
        levels = c("Poor", "Fair", "Good", "Excellent"),
        genders, classes)
    expect_equal(result, expected)
  })

  it("works without censoring and with gender split", {
    expected <- tibble::tribble(
      ~class, ~gender,  ~answer,      ~numerator, ~denom,
      "S1",   "Boys",   "Excellent",  1,          5,
      "S1",   "Boys",   "Good",       2,          5,
      "S1",   "Boys",   "Fair",       1,          5,
      "S1",   "Boys",   "Poor",       1,          5,
      "S1",   "Girls",  "Excellent",  1,          7,
      "S1",   "Girls",  "Good",       1,          7,
      "S1",   "Girls",  "Fair",       1,          7,
      "S1",   "Girls",  "Poor",       4,          7,
      # "S6",   "Boys",   "Excellent",  0,          3,
      "S6",   "Boys",   "Good",       1,          3,
      "S6",   "Boys",   "Fair",       1,          3,
      "S6",   "Boys",   "Poor",       1,          3,
      "S6",   "Girls",  "Excellent",  1,          5,
      "S6",   "Girls",  "Good",       2,          5,
      "S6",   "Girls",  "Fair",       1,          5,
      "S6",   "Girls",  "Poor",       1,          5,
      "All",  "All",    "Excellent",  3,          20,
      "All",  "All",    "Good",       6,          20,
      "All",  "All",    "Fair",       4,          20,
      "All",  "All",    "Poor",       7,          20,
    ) |>
      mutate(class = forcats::fct_inorder(class),
        answer = factor(answer, levels = c("Poor", "Fair", "Good", "Excellent")))
    result <-
      create_full_summary(input_data(),
        health,
        levels = c("Poor", "Fair", "Good", "Excellent"),
        genders, classes,
        .gender_split = TRUE)
    expect_equal(result, expected)
  })
})

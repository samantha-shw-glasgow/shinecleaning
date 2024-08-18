test_that("multiplication works", {
  set.seed(1)
  input_data <- tibble(
    gender = sample(c("Girl", "Boy"), 20, TRUE),
    class = sample(c("S1", "S6"), 20, TRUE),
    answer = sample(c("Yes", "No"), 20, TRUE),
  )
  expected <- tibble::tribble(
    ~class, ~gender,  ~numerator, ~denom,
    "S1",   "Boy",    2,          5,
    "S1",   "Girl",   2,          7,
    "S6",   "Boy",    1,          3,
    "S6",   "Girl",   2,          5,
    "All",  "All",    7,          20,
  )
  result <- create_summary(input_data, answer, "Yes")
  expect_equal(result, expected)
})

test_that("collapsed summary", {
  set.seed(1)
  input_data <- tibble(
    gender = sample(c("Girls", "Boys"), 20, TRUE),
    class = sample(c("S1", "S6"), 20, TRUE),
    answer = sample(c("Excellent", "Good", "Fair", "Poor"), 20, TRUE),
  ) |>
    arrange(desc(class))
  expected <- tibble::tribble(
    ~class, ~gender,  ~numerator, ~denom,
    "S1",   "Boys",   3,          5,
    "S1",   "Girls",  2,          7,
    "S6",   "Boys",   1,          3,
    "S6",   "Girls",  3,          5,
    "All",  "All",    9,          20,
  ) |>
    mutate(class = forcats::fct_inorder(class))
  result <- create_collapsed_summary(input_data, answer, c("Excellent", "Good"))
  expect_equal(result, expected)
})

test_that("full summary", {
  set.seed(1)
  input_data <- tibble(
    gender = sample(c("Girls", "Boys"), 20, TRUE),
    class = sample(c("S1", "S6"), 20, TRUE),
    answer = sample(c("Excellent", "Good", "Fair", "Poor"), 20, TRUE),
  )
  expected <- tibble::tribble(
    ~class, ~gender,  ~answer,      ~numerator, ~denom,
    "S1",   "Boys",   "Excellent",  1,          5,
    "S1",   "Boys",   "Fair",       1,          5,
    "S1",   "Boys",   "Good",       2,          5,
    "S1",   "Boys",   "Poor",       1,          5,
    "S1",   "Girls",  "Excellent",  1,          7,
    "S1",   "Girls",  "Fair",       1,          7,
    "S1",   "Girls",  "Good",       1,          7,
    "S1",   "Girls",  "Poor",       4,          7,
    # "S6",   "Boys",   "Excellent",  0,          3,
    "S6",   "Boys",   "Fair",       1,          3,
    "S6",   "Boys",   "Good",       1,          3,
    "S6",   "Boys",   "Poor",       1,          3,
    "S6",   "Girls",  "Excellent",  1,          5,
    "S6",   "Girls",  "Fair",       1,          5,
    "S6",   "Girls",  "Good",       2,          5,
    "S6",   "Girls",  "Poor",       1,          5,
    "All",  "All",    "Excellent",  3,          20,
    "All",  "All",    "Fair",       4,          20,
    "All",  "All",    "Good",       6,          20,
    "All",  "All",    "Poor",       7,          20,
  ) |>
    mutate(class = forcats::fct_inorder(class))
  result <- create_full_summary(input_data, answer, c("Excellent", "Good"))
  expect_equal(result, expected)
})

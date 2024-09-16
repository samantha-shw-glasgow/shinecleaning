test_that("share elevated split", {
  set.seed(1)
  input_data <- tibble(
    gender = sample(c("Girl", "Boy"), 30, TRUE),
    class = sample(c("P6", "P7"), 30, TRUE),
    mme_cat = sample(c("As expected", "Elevated"), 30, TRUE),
  )

  expected <- read_csv(test_path("examples/share_elevated_split.csv"),
    show_col_types = FALSE, col_types = "ccfiidd"
  )

  result <- share_elevated(input_data,
    mme_cat,
    .split = TRUE,
    .censor = TRUE,
    classes = c("P6", "P7"),
    genders = c("Boy", "Girl")
  )

  expect_equal(result, expected)
})

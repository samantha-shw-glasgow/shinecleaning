test_that("mean by multiple vars", {
  input_data <- tibble(
    class = "All",
    gender = "All",
    var1 = 1,
    var2 = 2,
    var3 = 3
  )

  expected <- list(tibble::tribble(
    ~gender, ~class, ~denom, ~var, ~mean, ~censored, ~labels, ~bar_lab_main, ~bar_lab_cens,
    "All pupils", "All", 1L, "var1", 1, 0, "Variable 1", "1.0", "",
    "All pupils", "All", 1L, "var2", 2, 0, "Variable 2", "2.0", "",
    "All pupils", "All", 1L, "var3", 3, 0, "Variable 3", "3.0", ""
  ) |>
    mutate(labels = fct_inorder(labels)))

  result <- summary_mean_multiple_vars(
    input_data,
    list(
      "var1" = "Variable 1",
      "var2" = "Variable 2",
      "var3" = "Variable 3"
    ),
    .gender_split = FALSE,
    class = "All",
    .censor = FALSE
  )

  expect_equal(result, expected)
})

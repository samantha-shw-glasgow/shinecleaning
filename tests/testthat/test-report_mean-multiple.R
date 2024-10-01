test_that("mean by multiple vars", {
  input_data <- tibble(
    class = "All",
    gender = "All",
    var1 = 1,
    var2 = 2,
    var3 = 3
  )

  input_data_bad <- bind_rows(
    input_data,
    tibble(class = "All", gender = "All")
  )

  input_data_pnts <- bind_rows(
    input_data |> mutate(across(everything(), as.character)),
    tibble(class = "All", gender = "All",
           var1 = "Prefer not to say",
           var2 = "Prefer not to say",
           var3 = "Prefer not to say")
  )

  expected <- list(tibble::tribble(
    ~gender, ~class, ~var, ~mean, ~denom, ~censored, ~labels, ~bar_lab_main, ~bar_lab_cens,
    "All pupils", "All", "var1", 1, 1L, 0, "Variable 1", "1.0", "",
    "All pupils", "All", "var2", 2, 1L, 0, "Variable 2", "2.0", "",
    "All pupils", "All", "var3", 3, 1L, 0, "Variable 3", "3.0", ""
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

  result_bad <- summary_mean_multiple_vars(
    input_data_bad,
    list(
      "var1" = "Variable 1",
      "var2" = "Variable 2",
      "var3" = "Variable 3"
    ),
    .gender_split = FALSE,
    class = "All",
    .censor = FALSE
  )

  result_pnts <- summary_mean_multiple_vars(
    input_data_pnts,
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
  expect_equal(result_bad, expected)
  expect_equal(result_pnts, expected)
})

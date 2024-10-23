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
    ~gender, ~class, ~var, ~mean, ~denominator, ~labels,
    "All pupils", "All", "var1", 1, 1L, "Variable 1",
    "All pupils", "All", "var2", 2, 1L, "Variable 2",
    "All pupils", "All", "var3", 3, 1L, "Variable 3",
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
    class = "All"
  )

  result_bad <- summary_mean_multiple_vars(
    input_data_bad,
    list(
      "var1" = "Variable 1",
      "var2" = "Variable 2",
      "var3" = "Variable 3"
    ),
    .gender_split = FALSE,
    class = "All"
  )

  result_pnts <- summary_mean_multiple_vars(
    input_data_pnts,
    list(
      "var1" = "Variable 1",
      "var2" = "Variable 2",
      "var3" = "Variable 3"
    ),
    .gender_split = FALSE,
    class = "All"
  )

  expect_equal(result, expected)
  expect_equal(result_bad, expected)
  expect_equal(result_pnts, expected)
})

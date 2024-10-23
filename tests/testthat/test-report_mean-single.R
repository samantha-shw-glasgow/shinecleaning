test_that("mean single summary works", {
  input_data <- tibble::tribble(
    ~class, ~gender, ~score,
    "S1", "Boys", 1,
    "S2", "Boys", 1,
    "S3", "Boys", 1,
    "S4", "Boys", 2,
    "S5", "Boys", 2,
    "S6", "Boys", 2,
    "S1", "Girls", 3,
    "S2", "Girls", 3,
    "S3", "Girls", 3,
    "S4", "Girls", 4,
    "S5", "Girls", 4,
    "S6", "Girls", 4
  ) |>
    mutate(score = as.character(score))

  input_data_bad <- tibble::tribble(
    ~class, ~gender, ~score,
    "S1", "Boys", 1,
    "S2", "Boys", 1,
    "S3", "Boys", 1,
    "S4", "Boys", 2,
    "S5", "Boys", 2,
    "S6", "Boys", 2,
    "S1", "Girls", 3,
    "S2", "Girls", 3,
    "S3", "Girls", 3,
    "S4", "Girls", 4,
    "S5", "Girls", 4,
    "S6", "Girls", 4,
    "S6", "Girls", NA_integer_
  ) |>
    mutate(score = as.character(score))

  input_data_pnts <- tibble::tribble(
    ~class, ~gender, ~score,
    "S1", "Boys", "1",
    "S2", "Boys", "1",
    "S3", "Boys", "1",
    "S4", "Boys", "2",
    "S5", "Boys", "2",
    "S6", "Boys", "2",
    "S1", "Girls", "3",
    "S2", "Girls", "3",
    "S3", "Girls", "3",
    "S4", "Girls", "4",
    "S5", "Girls", "4",
    "S6", "Girls", "4",
    "S6", "Girls", "Prefer not to say"
  )

  input_data_gender_pnts <- tibble::tribble(
    ~class, ~gender, ~score,
    "S1", "Boys", "1",
    "S2", "Boys", "1",
    "S3", "Boys", "1",
    "S4", "Boys", "2",
    "S5", "Boys", "2",
    "S6", "Boys", "2",
    "S1", "Girls", "3",
    "S2", "Girls", "3",
    "S3", "Girls", "3",
    "S4", "Girls", "4",
    "S5", "Girls", "4",
    "S6", "Girls", "4",
    "S6", "Prefer not to say", "4"
  )

  input_data_class_na <- tibble::tribble(
    ~class, ~gender, ~score,
    "S1", "Boys", "1",
    "S2", "Boys", "1",
    "S3", "Boys", "1",
    "S4", "Boys", "2",
    "S5", "Boys", "2",
    "S6", "Boys", "2",
    "S1", "Girls", "3",
    "S2", "Girls", "3",
    "S3", "Girls", "3",
    "S4", "Girls", "4",
    "S5", "Girls", "4",
    "S6", "Girls", "4",
    NA_character_, "Girls", "4"
  )

  expected_twogroup <- tibble::tribble(
    ~gender, ~mean_score, ~denom, ~class,
    "All", 2.5, 12, "All",
    "Boys", 1.0, 3, "S1, S2 and S3",
    "Boys", 2.0, 3, "S4, S5 and S6",
    "Girls", 3.0, 3, "S1, S2 and S3",
    "Girls", 4.0, 3, "S4, S5 and S6",
  ) |>
    mutate(class = factor(class, levels = c("S1, S2 and S3", "S4, S5 and S6", "All")))

  expected_twogroup_one_nd <- tibble::tribble(
    ~gender, ~mean_score, ~denom, ~class,
    "All", quiet_means(input_data_gender_pnts$score), 13, "All",
    "Boys", 1.0, 3, "S1, S2 and S3",
    "Boys", 2.0, 3, "S4, S5 and S6",
    "Girls", 3.0, 3, "S1, S2 and S3",
    "Girls", 4.0, 3, "S4, S5 and S6",
  ) |>
    mutate(class = factor(class, levels = c("S1, S2 and S3", "S4, S5 and S6", "All")))

  result_twogroup <- input_data |>
    summary_mean_single_var(
      score,
      classes = list(
        c("S1", "S2", "S3"),
        c("S4", "S5", "S6")
      )
    )

  result_twogroup_bad <- input_data_bad |>
    summary_mean_single_var(
      score,
      classes = list(
        c("S1", "S2", "S3"),
        c("S4", "S5", "S6")
      )
    )

  result_twogroup_pnts <- input_data_pnts |>
    summary_mean_single_var(
      score,
      classes = list(
        c("S1", "S2", "S3"),
        c("S4", "S5", "S6")
      )
    )

  result_twogroup_gender_pnts <- input_data_gender_pnts |>
    summary_mean_single_var(
      score,
      classes = list(
        c("S1", "S2", "S3"),
        c("S4", "S5", "S6")
      )
    )

  result_twogroup_class_na <- input_data_class_na |>
    summary_mean_single_var(
      score,
      classes = list(
        c("S1", "S2", "S3"),
        c("S4", "S5", "S6")
      )
    )

  expect_equal(result_twogroup, expected_twogroup)
  expect_equal(result_twogroup_bad, expected_twogroup)
  expect_equal(result_twogroup_pnts, expected_twogroup)
  expect_equal(result_twogroup_gender_pnts, expected_twogroup_one_nd)
  expect_equal(result_twogroup_class_na, expected_twogroup_one_nd)

  expected_sixgroup <- bind_rows(
    tibble::tribble(
      ~gender, ~mean_score, ~denom, ~class,
      "All", 2.5, 12, "All",
    ),
    input_data |> mutate(
      gender = gender,
      mean_score = as.numeric(score),
      denom = n(),
      class = class,
      .by = c("gender", "class"), .keep = "none"
    )
  ) |>
    mutate(class = factor(class, levels = c("S1", "S2", "S3", "S4", "S5", "S6", "All")))


  result_sixgroup <- input_data |>
    summary_mean_single_var(
      score,
      classes = c("S1", "S2", "S3", "S4", "S5", "S6")
    )

  expect_equal(result_sixgroup, expected_sixgroup)
})

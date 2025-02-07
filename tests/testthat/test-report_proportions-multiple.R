input_data <- function(bad_val = NULL, gender = "Girls", class = "P7") {
  out <- tibble::tribble(
    ~gender, ~class, ~health1, ~health2,
    "Boys", "P6", "Good", "Good",
    "Boys", "P6", "Good", "Poor",
    "Boys", "P6", "Poor", "Poor",
    "Boys", "P6", "Poor", "Poor",
    "Boys", "P7", "Good", "Good",
    "Boys", "P7", "Good", "Good",
    "Boys", "P7", "Good", "Poor",
    "Boys", "P7", "Poor", "Poor",
    "Girls", "P6", "Good", "Poor",
    "Girls", "P6", "Good", "Poor",
    "Girls", "P6", "Good", "Good",
    "Girls", "P6", "Good", "Good",
    "Girls", "P7", "Good", "Poor",
    "Girls", "P7", "Good", "Poor",
    "Girls", "P7", "Poor", "Poor",
    "Girls", "P7", "Poor", "Poor"
  )

  if (!is.null(bad_val)) {
    dplyr::bind_rows(
      out,
      tibble::tibble(gender = gender, class = class, health1 = bad_val, health2 = bad_val)
    )
  } else {
    out
  }

}

describe("report multiple proportions", {

  expected_p6 <- tibble::tribble(
    ~gender, ~class, ~denominator, ~var, ~numerator, ~labels, ~prop,
    "Boys", "P6", 4, "health1", 2, "Health var 1", 0.5,
    "Boys", "P6", 4, "health2", 1, "Health var 2", 0.25,
    "Girls", "P6", 4, "health1", 4, "Health var 1", 1,
    "Girls", "P6", 4, "health2", 2, "Health var 2", 0.5,
  )
  expected_p7 <- tibble::tribble(
    ~gender, ~class, ~denominator, ~var, ~numerator, ~labels, ~prop,
    "Boys", "P7", 4, "health1", 3, "Health var 1", 0.75,
    "Boys", "P7", 4, "health2", 2, "Health var 2", 0.5,
    "Girls", "P7", 4, "health1", 2, "Health var 1", 0.5,
    "Girls", "P7", 4, "health2", 0, "Health var 2", 0.0,
  )
  expected_all <- tibble::tribble(
    ~gender, ~class, ~denominator, ~var, ~numerator, ~labels, ~prop,
    "All", "All", 16, "health1", 11, "Health var 1", 11 / 16,
    "All", "All", 16, "health2",  5, "Health var 2", 5 / 16,
  )

  expected_all_oneclass <- tibble::tribble(
    ~gender, ~class, ~denominator, ~var, ~numerator, ~labels, ~prop,
    "All", "All", 8, "health1", 5, "Health var 1", 5 / 8,
    "All", "All", 8, "health2",  2, "Health var 2", 2 / 8,
  )

  expected_c <- list(expected_p6, expected_p7, expected_all) |>
    purrr::map(~dplyr::mutate(.x, labels = factor(labels) |> forcats::fct_rev()))

  expected_one_class <- list(expected_p7, expected_all_oneclass) |>
    purrr::map(~dplyr::mutate(.x, labels = factor(labels) |> forcats::fct_rev()))

  classes <- c("P6", "P7")

  it("reports correctly", {

    result <-
      summary_proportions_multiple(
        input_data(),
        list(health1 = "Health var 1", health2 = "Health var 2"),
        success = ~ .x == "Good",
        classes = classes
      )

    expect_equal(result, expected_c)

  })

  it("handles NA variables", {

    result_bad <-
      summary_proportions_multiple(
        input_data(NA_character_),
        list(health1 = "Health var 1", health2 = "Health var 2"),
        success = ~ .x == "Good",
        classes = classes
      )

    expect_equal(result_bad, expected_c)


  })

  it("handles pnts variables", {

    result_pnts <-
      summary_proportions_multiple(
        input_data("Prefer not to say"),
        list(health1 = "Health var 1", health2 = "Health var 2"),
        success = ~ .x == "Good",
        classes = classes
      )

    expect_equal(result_pnts, expected_c)


  })

  expected_all_one_not_declared <- tibble::tribble(
      ~gender, ~class, ~denominator, ~var, ~numerator, ~labels, ~prop,
      "All", "All", 17, "health1", 12, "Health var 1", 12 / 17,
      "All", "All", 17, "health2",  6, "Health var 2", 6 / 17,
    )

  expected_one_nd <- list(expected_p6, expected_p7, expected_all_one_not_declared) |>
    purrr::map(~dplyr::mutate(.x, labels = factor(labels) |> forcats::fct_rev()))


  it("handles class not declared", {
    result_one_class_missing <-
      summary_proportions_multiple(
        input_data("Good", "Girls", "Prefer not to say"),
        list(health1 = "Health var 1", health2 = "Health var 2"),
        success = ~ .x == "Good",
        classes = classes
      )

    expect_equal(result_one_class_missing, expected_one_nd)
  })


  it("handles gender not declared", {

    result_one_gender_missing <-
      summary_proportions_multiple(
        input_data("Good", "Prefer not to say", "P7"),
        list(health1 = "Health var 1", health2 = "Health var 2"),
        success = ~ .x == "Good",
        classes = classes
      )

    expect_equal(result_one_gender_missing, expected_one_nd)


  })

  it("handles missing classes", {

    result_missing_class <-
      summary_proportions_multiple(
        input_data() |> dplyr::filter(class == "P7"),
        list(health1 = "Health var 1", health2 = "Health var 2"),
        success = ~ .x == "Good",
        classes = classes
      )

    expect_equal(result_missing_class, expected_one_class)
  })

})

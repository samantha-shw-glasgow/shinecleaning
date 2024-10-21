input_data <- function(bad_val = NULL, gender = "Girls", class = "P7") {
  set.seed(1)
  out <- tibble(
    gender = sample(c("Girl", "Boy"), 30, TRUE),
    class = sample(c("P6", "P7"), 30, TRUE),
    mme_cat = sample(c("As expected", "Elevated"), 30, TRUE),
  )

  if (!is.null(bad_val)) {
    bind_rows(
      out,
      tibble(gender = gender, class = class, mme_cat = bad_val)
    )
  } else {
    out
  }

}

expected <- read_csv(
  test_path("examples/share_elevated_split.csv"),
  show_col_types = FALSE,
  col_types = "ccfiidd"
)

expected_missing_one <- read_csv(
  test_path("examples/share_elevated_split.csv"),
  show_col_types = FALSE,
  col_types = "ccfiidd"
) |>
  mutate(
    n = if_else(gender == "All" & var == "Elevated", n + 1, n),
    denom = if_else(gender == "All", denom + 1, denom),
    prop = n / denom
  )

describe("share elevated split", {
  it("correctly calculates shares elevated", {

    result <- share_elevated(
      input_data(),
      mme_cat,
      .split = TRUE,
      classes = c("P6", "P7"),
      genders = c("Boy", "Girl")
    )

    expect_equal(result, expected)

  })

  it("handles NA values", {

    result_bad <- share_elevated(
      input_data(NA_character_),
      mme_cat,
      .split = TRUE,
      classes = c("P6", "P7"),
      genders = c("Boy", "Girl")
    )

    expect_equal(result_bad, expected)

  })

  it("handles pnts values", {

    result_pnts <- share_elevated(
      input_data("Prefer not to say"),
      mme_cat,
      .split = TRUE,
      classes = c("P6", "P7"),
      genders = c("Boy", "Girl")
    )

    expect_equal(result_pnts, expected)

  })

  it("handles no class declared", {

    result_noclass <- share_elevated(
      input_data("Elevated", class = NA_character_),
      mme_cat,
      .split = TRUE,
      classes = c("P6", "P7"),
      genders = c("Boy", "Girl")
    )

    expect_equal(result_noclass, expected_missing_one)
  })

  it("handles no gender declared", {

    result_nogender <- share_elevated(
      input_data("Elevated", gender = NA_character_),
      mme_cat,
      .split = TRUE,
      classes = c("P6", "P7"),
      genders = c("Boy", "Girl")
    )

    expect_equal(result_nogender, expected_missing_one)
  })

})

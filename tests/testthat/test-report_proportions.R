input_data <- function(bad_val = NULL, gender = "Girls", class = "S1") {
  out <- tibble::tribble(
    ~gender, ~class, ~health,
    "Girls", "S6", "Good",
    "Boys", "S6", "Fair",
    "Girls", "S6", "Good",
    "Girls", "S6", "Excellent",
    "Girls", "S6", "Poor",
    "Boys", "S6", "Poor",
    "Boys", "S6", "Good",
    "Girls", "S6", "Fair",
    "Girls", "S1", "Good",
    "Boys", "S1", "Good",
    "Girls", "S1", "Fair",
    "Girls", "S1", "Poor",
    "Boys", "S1", "Poor",
    "Girls", "S1", "Poor",
    "Girls", "S1", "Poor",
    "Boys", "S1", "Excellent",
    "Girls", "S1", "Poor",
    "Girls", "S1", "Excellent",
    "Boys", "S1", "Fair",
    "Boys", "S1", "Good"
  )

  if (!is.null(bad_val)) {
    bind_rows(
      out,
      tibble(gender = gender, class = class, health = bad_val)
    )
  } else {
    out
  }

}

input_data_full <- function(bad_val = NULL) {

  set.seed(100)

  out <- tibble(
    class = rep(paste0("S", 1:6), each = 20),
    gender = rep(c("Boys", "Girls"), times = 60),
    health = sample(c("Excellent", "Fair", "Good", "Poor"), 120, replace = TRUE)
  )

  if (!is.null(bad_val)) {
    bind_rows(
      out,
      tibble(class = "S1", gender = "Girls", health = bad_val)
    )
  } else {
    out
  }
}

classes <- c("S1", "S2", "S3", "S4", "S5", "S6")
genders <- c("Boys", "Girls", "All")
classes_s2 <- list(
  c("S1", "S2", "S3"),
  c("S4", "S5", "S6")
)

describe("collapsed summary", {
  it("works without gender split", {
    expected <- tibble::tribble(
      ~class, ~gender, ~numerator, ~denominator,
      # "S1",   "All",    5,          12,
      # "S6",   "All",    4,          8,
      "All", "All pupils", 9, 20,
    ) |>
      mutate(class = forcats::fct_inorder(class))
    result <- create_collapsed_summary(
      input_data(),
      health,
      c("Excellent", "Good"),
      genders, classes,
      .gender_split = FALSE
    )
    expect_equal(result, expected)
  })

  it("works with gender split", {
    expected <- tibble::tribble(
      ~class, ~gender,  ~numerator, ~denominator,
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

  it("Ignores NA values", {
    expected <- tibble::tribble(
      ~class, ~gender,  ~numerator, ~denominator,
      "S1",   "Boys",   3,          5,
      "S1",   "Girls",  2,          7,
      "S6",   "Boys",   1,          3,
      "S6",   "Girls",  3,          5,
      "All",  "All",    9,          20,
    ) |>
      mutate(class = forcats::fct_inorder(class))
    result <- create_collapsed_summary(
      input_data(NA_character_),
      health,
      c("Excellent", "Good"),
      genders, classes,
      .gender_split = TRUE
    )
    expect_equal(result, expected)
  })

  it("Ignores pnts values", {
    expected <- tibble::tribble(
      ~class, ~gender,  ~numerator, ~denominator,
      "S1",   "Boys",   3,          5,
      "S1",   "Girls",  2,          7,
      "S6",   "Boys",   1,          3,
      "S6",   "Girls",  3,          5,
      "All",  "All",    9,          20,
    ) |>
      mutate(class = forcats::fct_inorder(class))
    result <- create_collapsed_summary(
      input_data("Prefer not to say"),
      health,
      c("Excellent", "Good"),
      genders, classes,
      .gender_split = TRUE
    )
    expect_equal(result, expected)
  })

  it("works as 2-grouped classes", {

    expected <- input_data_full() |>
      mutate(class = forcats::fct_collapse(class,
        "S1, S2 and S3" = classes_s2[[1]],
        "S4, S5 and S6" = classes_s2[[2]]
      ) |> as.character()) |>
      summarise(
        numerator = sum(health %in% c("Excellent", "Good")),
        denominator = n(),
        .by = c("class", "gender")
      ) |>
      (
        \(d) summarise(d, across(c(
          numerator, denominator
        ), sum)) |> mutate(gender = "All", class = "All") |> bind_rows(d, x = _)
      )() |>
      mutate(class = forcats::fct_inorder(class))

    result <- create_collapsed_summary(
      input_data_full(),
      health,
      c("Excellent", "Good"),
      genders, classes_s2,
      .gender_split = TRUE
    )

    expect_equal(result, expected)

  })

})

describe("full summary", {
  it("works without gender split", {
    expected <- tibble::tribble(
      ~class, ~gender,  ~answer,      ~numerator, ~denominator,
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
      mutate(
        class = forcats::fct_inorder(class),
        answer = factor(answer, levels = c("Poor", "Fair", "Good", "Excellent"))
      )
    result <-
      create_full_summary(input_data(),
        health,
        levels = c("Poor", "Fair", "Good", "Excellent"),
        genders, classes
      )
    expect_equal(result, expected)
  })

  it("works with gender split", {
    expected <- tibble::tribble(
      ~class, ~gender,  ~answer,      ~numerator, ~denominator,
      "S1",   "Boys",   "Excellent",  1,          5,
      "S1",   "Boys",   "Good",       2,          5,
      "S1",   "Boys",   "Fair",       1,          5,
      "S1",   "Boys",   "Poor",       1,          5,
      "S1",   "Girls",  "Excellent",  1,          7,
      "S1",   "Girls",  "Good",       1,          7,
      "S1",   "Girls",  "Fair",       1,          7,
      "S1",   "Girls",  "Poor",       4,          7,
      "S6",   "Boys",   "Excellent",  0,          3,
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
      mutate(
        class = forcats::fct_inorder(class),
        answer = factor(answer, levels = c("Poor", "Fair", "Good", "Excellent"))
      )
    result <-
      create_full_summary(input_data(),
        health,
        levels = c("Poor", "Fair", "Good", "Excellent"),
        genders, classes,
        .gender_split = TRUE
      )
    expect_equal(result, expected)
  })

  it("works as 2-grouped classes", {

    expected <- input_data_full() |>
      mutate(
        class = forcats::fct_collapse(class, "S1, S2 and S3" = classes_s2[[1]], "S4, S5 and S6" = classes_s2[[2]]) |> as.character()
      ) |>
      summarise(numerator = n(),
                .by = c("class", "gender", "health")) |>
      mutate(denominator = sum(numerator),
             .by = c("gender", "class")) |>
      (
        \(d) summarise(d, across(c(
          numerator, denominator
        ), sum), .by = "health") |> mutate(gender = "All", class = "All") |> bind_rows(d, x = _)
      )() |>
      mutate(
        class = forcats::fct_inorder(class),
        health = factor(health, levels = c("Poor", "Fair", "Good", "Excellent"))
      ) |>
      rename(answer = health) |>
      arrange(class, gender, desc(answer))

    result <- create_full_summary(
      input_data_full(),
      health,
      levels = c("Poor", "Fair", "Good", "Excellent"),
      genders, classes_s2,
      .gender_split = TRUE
    )

    expect_equal(result, expected)

  })

  it("works as 2-grouped classes, ignoring NA values", {

    expected <- input_data_full() |>
      mutate(
        class = forcats::fct_collapse(class, "S1, S2 and S3" = classes_s2[[1]], "S4, S5 and S6" = classes_s2[[2]]) |> as.character()
      ) |>
      summarise(numerator = n(),
                .by = c("class", "gender", "health")) |>
      mutate(denominator = sum(numerator),
             .by = c("gender", "class")) |>
      (
        \(d) summarise(d, across(c(
          numerator, denominator
        ), sum), .by = "health") |> mutate(gender = "All", class = "All") |> bind_rows(d, x = _)
      )() |>
      mutate(
        class = forcats::fct_inorder(class),
        health = factor(health, levels = c("Poor", "Fair", "Good", "Excellent"))
      ) |>
      rename(answer = health) |>
      arrange(class, gender, desc(answer))

    result <- create_full_summary(
      input_data_full(NA_character_),
      health,
      levels = c("Poor", "Fair", "Good", "Excellent"),
      genders, classes_s2,
      .gender_split = TRUE
    )

    expect_equal(result, expected)

  })

  it("ignores NA rows", {

    expected <- tibble::tribble(
      ~class, ~gender,  ~answer,      ~numerator, ~denominator,
      "S1",   "Boys",   "Excellent",  1,          5,
      "S1",   "Boys",   "Good",       2,          5,
      "S1",   "Boys",   "Fair",       1,          5,
      "S1",   "Boys",   "Poor",       1,          5,
      "S1",   "Girls",  "Excellent",  1,          7,
      "S1",   "Girls",  "Good",       1,          7,
      "S1",   "Girls",  "Fair",       1,          7,
      "S1",   "Girls",  "Poor",       4,          7,
      "S6",   "Boys",   "Excellent",  0,          3,
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
      mutate(
        class = forcats::fct_inorder(class),
        answer = factor(answer, levels = c("Poor", "Fair", "Good", "Excellent"))
      )
    result <-
      create_full_summary(input_data(NA_character_),
                          health,
                          levels = c("Poor", "Fair", "Good", "Excellent"),
                          genders, classes,
                          .gender_split = TRUE
      )
    expect_equal(result, expected)

  })

  it("ignores pnts rows", {

    expected <- tibble::tribble(
      ~class, ~gender,  ~answer,      ~numerator, ~denominator,
      "S1",   "Boys",   "Excellent",  1,          5,
      "S1",   "Boys",   "Good",       2,          5,
      "S1",   "Boys",   "Fair",       1,          5,
      "S1",   "Boys",   "Poor",       1,          5,
      "S1",   "Girls",  "Excellent",  1,          7,
      "S1",   "Girls",  "Good",       1,          7,
      "S1",   "Girls",  "Fair",       1,          7,
      "S1",   "Girls",  "Poor",       4,          7,
      "S6",   "Boys",   "Excellent",  0,          3,
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
      mutate(
        class = forcats::fct_inorder(class),
        answer = factor(answer, levels = c("Poor", "Fair", "Good", "Excellent"))
      )
    result <-
      create_full_summary(input_data("Prefer not to say"),
                          health,
                          levels = c("Poor", "Fair", "Good", "Excellent"),
                          genders, classes,
                          .gender_split = TRUE
      )
    expect_equal(result, expected)

  })

  it("ignores missing class in all but 'All'", {
    expected <- tibble::tribble(
      ~class, ~gender,  ~answer,      ~numerator, ~denominator,
      "S1",   "Boys",   "Excellent",  1,          5,
      "S1",   "Boys",   "Good",       2,          5,
      "S1",   "Boys",   "Fair",       1,          5,
      "S1",   "Boys",   "Poor",       1,          5,
      "S1",   "Girls",  "Excellent",  1,          7,
      "S1",   "Girls",  "Good",       1,          7,
      "S1",   "Girls",  "Fair",       1,          7,
      "S1",   "Girls",  "Poor",       4,          7,
      "S6",   "Boys",   "Excellent",  0,          3,
      "S6",   "Boys",   "Good",       1,          3,
      "S6",   "Boys",   "Fair",       1,          3,
      "S6",   "Boys",   "Poor",       1,          3,
      "S6",   "Girls",  "Excellent",  1,          5,
      "S6",   "Girls",  "Good",       2,          5,
      "S6",   "Girls",  "Fair",       1,          5,
      "S6",   "Girls",  "Poor",       1,          5,
      "All",  "All",    "Excellent",  3,          21,
      "All",  "All",    "Good",       6,          21,
      "All",  "All",    "Fair",       4,          21,
      "All",  "All",    "Poor",       8,          21,
    ) |>
      mutate(
        class = forcats::fct_inorder(class),
        answer = factor(answer, levels = c("Poor", "Fair", "Good", "Excellent"))
      )
    result <-
      create_full_summary(input_data("Poor", "Girls", "Prefer not to say"),
                          health,
                          levels = c("Poor", "Fair", "Good", "Excellent"),
                          genders, classes,
                          .gender_split = TRUE
      )
    expect_equal(result, expected)


  })

  it("ignores missing gender in all but 'All'", {
    expected <- tibble::tribble(
      ~class, ~gender,  ~answer,      ~numerator, ~denominator,
      "S1",   "Boys",   "Excellent",  1,          5,
      "S1",   "Boys",   "Good",       2,          5,
      "S1",   "Boys",   "Fair",       1,          5,
      "S1",   "Boys",   "Poor",       1,          5,
      "S1",   "Girls",  "Excellent",  1,          7,
      "S1",   "Girls",  "Good",       1,          7,
      "S1",   "Girls",  "Fair",       1,          7,
      "S1",   "Girls",  "Poor",       4,          7,
      "S6",   "Boys",   "Excellent",  0,          3,
      "S6",   "Boys",   "Good",       1,          3,
      "S6",   "Boys",   "Fair",       1,          3,
      "S6",   "Boys",   "Poor",       1,          3,
      "S6",   "Girls",  "Excellent",  1,          5,
      "S6",   "Girls",  "Good",       2,          5,
      "S6",   "Girls",  "Fair",       1,          5,
      "S6",   "Girls",  "Poor",       1,          5,
      "All",  "All",    "Excellent",  3,          21,
      "All",  "All",    "Good",       6,          21,
      "All",  "All",    "Fair",       4,          21,
      "All",  "All",    "Poor",       8,          21,
    ) |>
      mutate(
        class = forcats::fct_inorder(class),
        answer = factor(answer, levels = c("Poor", "Fair", "Good", "Excellent"))
      )
    result <-
      create_full_summary(input_data("Poor", "Prefer not to say", "S1"),
                          health,
                          levels = c("Poor", "Fair", "Good", "Excellent"),
                          genders, classes,
                          .gender_split = TRUE
      )
    expect_equal(result, expected)


  })
})

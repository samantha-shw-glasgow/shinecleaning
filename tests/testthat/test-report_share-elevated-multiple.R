input_data <- function(bad_val = NULL, gender = "Girls", class = "P7") {
  set.seed(1)
  out <- tibble(
    gender = sample(c("Girls", "Boys"), 30, TRUE),
    class = sample(c("P6", "P7"), 30, TRUE),
    cat1 = sample(c("As expected", "Elevated"), 30, TRUE),
    cat2 = sample(c("As expected", "Elevated"), 30, TRUE)
  )

  if (!is.null(bad_val)) {
    bind_rows(
      out,
      tibble(gender = gender, class = class, cat1 = bad_val[1], cat2 = bad_val[2])
    )
  } else {
    out
  }

}

classes <- c("P6", "P7")
levels <-  c("As expected", "Elevated")
genders <- c("Boys", "Girls")
varlist <- list(
  cat1 = "Variable 1",
  cat2 = "Variable 2"
)

expected_outs <- function(...) {
  subgs <- map(classes, \(inc_class) {
    map(levels, \(level) {
      input_data(...) |>
        filter(class %in% inc_class, gender %in% genders) |>
        pivot_longer(c(cat1, cat2),
                     names_to = "var",
                     values_to = "val") |>
        summarise(val = sum(val %in% level),
                  .by = c("gender", "var")) |>
        mutate(
          level = level,
          class = inc_class,
          var = paste(gender, varlist[var], sep = "-")
        )
    }) |>
      reduce(bind_rows) |>
      select(gender, class, var, level, n = val) |>
      arrange(gender, class, var) |>
      mutate(denom = sum(n),
             .by = c("gender", "class", "var")) |>
      mutate(
        prop = n / denom,
        level = factor(level, levels = levels)
      )

  })

  all <- map(levels, \(level) {
    input_data(...) |>
      pivot_longer(c(cat1, cat2), names_to = "var", values_to = "val") |>
      summarise(val = sum(val %in% level), .by = c("var")) |>
      mutate(
        level = level,
        class = "All",
        gender = "All",
        var = paste(varlist[var])
      )
  }) |>
    reduce(bind_rows) |>
    select(gender, class, var, level, n = val) |>
    arrange(gender, class, var) |>
    mutate(denom = sum(n),
           .by = c("gender", "class", "var")) |>
    mutate(
      prop = n / denom,
      level = factor(level, levels = levels)
    )

  append(subgs, list(all))
}

describe("share elevated - multiple variables", {

  it("calculates correctly", {
    expected <- expected_outs()

    result <- share_elevated_multiple(
      input_data(),
      varlist = varlist,
      classes = classes,
      genders = c("Boys", "Girls"),
      .split = TRUE
    )

    expect_equal(result, expected)
  })

  it("handles NA", {
    expected_bad <- expected_outs(bad_val = c(NA_character_, NA_character_))

    result_bad <- share_elevated_multiple(
      input_data(bad_val = c(NA_character_, NA_character_)),
      varlist = varlist,
      classes = classes,
      genders = c("Boys", "Girls"),
      .split = TRUE
    )

    expect_equal(result_bad, expected_bad)
  })

  it("handles pnts", {
    expected_pnts <- expected_outs(bad_val = c("Prefer not to say", "Elevated"))

    result_pnts <- share_elevated_multiple(
      input_data(bad_val = c("Prefer not to say", "Elevated")),
      varlist = varlist,
      classes = classes,
      genders = c("Boys", "Girls"),
      .split = TRUE
    )

    expect_equal(result_pnts, expected_pnts)
  })

  it("handles no gender", {
    expected_nogender <- expected_outs(bad_val = c("Elevated", "Elevated"), gender = "Prefer not to say")

    result_nogender <- share_elevated_multiple(
      input_data(bad_val = c("Elevated", "Elevated"), gender = "Prefer not to say"),
      varlist = varlist,
      classes = classes,
      genders = c("Boys", "Girls"),
      .split = TRUE
    )

    expect_equal(result_nogender, expected_nogender)
  })

  it("handles no class", {
    expected_noclass <- expected_outs(bad_val = c("Elevated", "Elevated"), class = "Prefer not to say")

    result_noclass <- share_elevated_multiple(
      input_data(bad_val = c("Elevated", "Elevated"), class = "Prefer not to say"),
      varlist = varlist,
      classes = classes,
      genders = c("Boys", "Girls"),
      .split = TRUE
    )

    expect_equal(result_noclass, expected_noclass)
  })

  it("handles missing class", {
    expected_missing_class <-
      list(expected_outs()[2][[1]],
           expected_outs()[2][[1]] |>
             mutate(
               gender = "All",
               class = "All",
               var = str_extract(var, "Variable \\d")) |>
             summarise(
               n = sum(n),
               denom = sum(denom),
               .by = c("gender", "class", "var", "level")
             ) |>
             mutate(prop = n / denom) |>
             select(gender, class, var, level, n, denom, prop)
    )

    result_missing_class <- share_elevated_multiple(
      input_data() |> filter(class == "P7"),
      varlist = varlist,
      classes = classes,
      genders = c("Boys", "Girls"),
      .split = TRUE
    )

    expect_equal(result_missing_class, expected_missing_class)



  })

})

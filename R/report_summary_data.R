create_collapsed_summary <- function(
 data,
 var,
 success,
 .censor = FALSE,
 .gender_split = FALSE
) {
  var <- enquo(var)

  subgroups <- data |>
    mutate(gender = case_match(
      gender,
      "Boy" ~ "Boys",
      "Girl" ~ "Girls"
    )) |>
    group_by(class, gender) |>
    mutate(success = {{var}} %in% success) |>
    summarise(
      numerator = sum(success, na.rm = TRUE),
      denom = n(),
      .groups = "drop"
    )
  all <- subgroups |>
    summarise(
      class = "All",
      gender = "All",
      numerator = sum(numerator),
      denom = sum(denom)
    )
  bind_rows(subgroups, all)
}

create_full_summary <- function(
    data,
    var,
    .censor = FALSE,
    .gender_split = FALSE
) {
  var <- enquo(var)

  subgroups <- data |>
    mutate(gender = case_match(
      gender,
      "Boy" ~ "Boys",
      "Girl" ~ "Girls"
    )) |>
    group_by(class, gender, !!var) |>
    summarise(numerator = n(), .groups = "drop") |>
    add_count(class, gender, name = "denom", wt = numerator)
  all <- subgroups |>
    summarise(
      class = "All",
      gender = "All",
      numerator = sum(numerator),
      .by = !!var
    ) |>
    mutate(denom = sum(numerator))
  bind_rows(subgroups, all)
}

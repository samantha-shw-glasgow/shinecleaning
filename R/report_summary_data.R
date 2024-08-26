create_collapsed_summary <- function(
 data,
 var,
 success,
 .censor = FALSE,
 .gender_split = FALSE
) {

  subgroups <- data |>
    group_by(class, gender) |>
    mutate(success = {{var}} %in% success) |>
    summarise(
      numerator = sum(success, na.rm = TRUE),
      denom = n(),
      .groups = "drop"
    ) |>
    arrange(class)
  all <- subgroups |>
    summarise(
      class = "All",
      gender = "All",
      numerator = sum(numerator),
      denom = sum(denom)
    )

  bind_rows(subgroups, all) |>
    mutate(class = forcats::fct_inorder(class))
}

create_full_summary <- function(
    data,
    var,
    .censor = FALSE,
    .gender_split = FALSE
) {
  var <- enquo(var)

  subgroups <- data |>
    rename(answer = !!var) |>
    group_by(class, gender, answer) |>
    summarise(numerator = n(), .groups = "drop") |>
    add_count(class, gender, name = "denom", wt = numerator) |>
    arrange(class)
  all <- subgroups |>
    summarise(
      class = "All",
      gender = "All",
      numerator = sum(numerator),
      .by = answer
    ) |>
    mutate(denom = sum(numerator))

  bind_rows(subgroups, all) |>
    mutate(class = forcats::fct_inorder(class))
}

create_summary <- function(
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
  all <- data |>
    mutate(success = {{var}} %in% success) |>
    summarise(
      class = "All",
      gender = "All",
      numerator = sum(success, na.rm = TRUE),
      denom = n()
    )
  bind_rows(subgroups, all)
}

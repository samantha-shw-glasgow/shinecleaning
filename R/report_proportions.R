#' Collapsed summary (percentages of successes) for proportion graphs
#'
#' @param data Valid input data
#' @param var Variable to calculate by
#' @param success Character vector of categories as 'successes'
#' @param .censor Whether to censor (must be TRUE for production reports)
#' @param .gender_split Gender split - passed from params
#'
#' @return A dataframe of proportions/counts of successes
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

#' Full summary counts across all categories
#'
#' @param data Valid input data
#' @param var Variable to calculate by
#' @param .censor Whether to censor (must be TRUE for production reports)
#' @param .gender_split Gender split - passed from params
#'
#' @return A dataframe of counted variables
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


#' Bar percentage from summary data
#'
#' @param summary_data A dataframe produced by `create_collapsed_summary`
#'
#' @return A ggplot2 graph
#' @examples
#' N = 100
#'tibble(
#'   gender = sample(c("Girl", "Boy"), N, TRUE),
#'   class = sample(c("S1", "S6"), N, TRUE),
#'   answer = sample(c("Excellent", "Good", "Fair", "Poor"), N, TRUE)
#' ) |>
#'   create_collapsed_summary(answer, success = c("Excellent", "Good")) |>
#'   bar_from_summary()
bar_from_summary <- function(summary_data) {
  summary_data |>
    filter(gender %in% c("Boys", "Girls", "All")) |>
    mutate(prop = numerator/denom) |>
    ggplot() +
    aes(x = class, y = prop, fill = gender) +
    geom_col(position = "dodge") +
    scale_fill_hbsc("") +
    xlab("") +
    scale_y_continuous("%", labels = scales::percent)+
    geom_text(aes(label = scales::percent(.data$prop, suffix="%", accuracy = 1)),
              position = position_dodge(0.9),
              vjust = -0.5,
              size = 4) +
    theme(plot.margin = unit(c(0.8, 0.5, 0.5, 1),  "cm")) +
    coord_cartesian(ylim = c(0, 1), clip = "off")
}

#' A table of percentages from summary data
#'
#' @param summary_data A dataframe produced by `create_collapsed_summary`
#'
#' @return A printed `flextable`
#'
#' @examples
#'
#' tibble(
#'     gender = sample(c("Girl", "Boy"), N, TRUE),
#'     class = sample(c("S1", "S6"), N, TRUE),
#'     answer = sample(c("Yes", "No"), N, TRUE),
#' ) |>
#'   create_full_summary(answer) |>
#'   table_from_summary()
#'
table_from_summary <- function(summary_data) {
  summary_data |>
    mutate(prop = numerator/denom) |>
    pivot_wider(id_cols = answer, names_from = c(class, gender), values_from = prop) |>
    flextable() |>
    separate_header()|>
    theme_vanilla() |>
    set_table_properties(layout = "autofit", width = 1) |>
    set_caption(align_with_table = FALSE)
}


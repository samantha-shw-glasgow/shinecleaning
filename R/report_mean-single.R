#' Summary table of means for a single variable
#'
#' @param data Valid input data
#' @param var Variable to calculate mean of
#' @param genders List of genders to split by
#' @param classes List of classes to split by
#' @param .gender_split `TRUE`/`FALSE` - split by gender when sufficient numbers of responses
#'
#' @return A summary table of means of a single variable
#'
#' @import dplyr
#' @importFrom rlang .data
#'
summary_mean_single_var <-
  function(data,
           var,
           genders = c("Boys", "Girls"),
           classes = "All",
           .gender_split = TRUE) {
    subgroups <- tibble()

    if (.gender_split) {
      subgroups <-
        map(classes, \(concat_class) {
          data |>
            filter(gender %in% genders, class %in% concat_class) |>
            summarise(
              mean_score = quiet_means({{ var }}),
              denom = how_many_valid(valid_numbers({{ var }})),
              class = str_flatten(concat_class, collapse = ", ", last = " and "),
              .by = c(gender)
            )
        }) |>
        reduce(bind_rows) |>
        arrange(class)
    }


    all <- data |>
      mutate(class = "All", gender = if_else(.gender_split, "All", "All pupils")) |>
      summarise(
        mean_score = quiet_means({{ var }}),
        denom = how_many_valid(valid_numbers({{ var }})),
        .by = c(class, gender)
      )

    bind_rows(subgroups, all) |>
      mutate(class = fct_inorder(class)) |>
      arrange(gender, class)
  }

#' Bar graph of mean of a single var
#'
#' @param summary_data A dataframe produced by `summary_mean_single`
#' @param ymax Upper limit of graph (deaults to `limits[2]`)
#' @param ylab Label for X axis (summary statistic, i.e. "mean")
#'
#' @returns A ggplot2 graph
bar_mean_single <- function(summary_data, ymax, ylab = "Mean") {
  summary_data |>
    mutate(
      mean_score = if_else(.data$censored, 1, .data$mean_score),
      bar_lab_main = if_else(.data$censored, "*", sprintf("%.1f", .data$mean_score)),
      bar_lab_cens = if_else(.data$censored, "Numbers too low to show", "")
    ) |>
    ggplot() +
    aes(x = class, y = mean_score, fill = gender, linetype = .data$censored, alpha = .data$censored) +
    geom_bar_t(
      stat = "identity",
      position = position_dodge(width = 0.7),
      linetype = "blank"
    ) +
    scale_x_discrete("") +
    scale_alpha_manual(values = c("TRUE" = 0.6, "FALSE" = 1), guide = guide_none()) +
    scale_linetype_manual(
      values = c("TRUE" = "dashed", "FALSE" = "blank"),
      guide = guide_none()
    ) +
    scale_fill_hbsc(name = "") +
    theme(
      legend.justification.right = "top",
      legend.title = element_blank(),
      plot.margin = unit(c(0.8, 1, 0.5, 0), "cm")
    ) +
    scale_y_continuous(ylab, expand = expansion(add = 0)) +
    geom_text(
      aes(label = .data$bar_lab_main),
      vjust = -0.5,
      colour = "black",
      position = position_dodge(width = 0.7),
      size = if_else(length(unique(summary_data$class)) > 3, 2.5, 4)
    ) +
    coord_cartesian(ylim = c(0, ymax), clip = "off") +
    labs(caption = if_else(any(summary_data$censored), "* Numbers too low to show", ""))
}

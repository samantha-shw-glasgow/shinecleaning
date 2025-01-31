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
#' @importFrom rlang .data
#'
summary_mean_single_var <-
  function(data,
           var,
           genders = c("Boys", "Girls"),
           classes = "All",
           .gender_split = TRUE) {
    subgroups <- tibble::tibble()

    if (.gender_split) {
      subgroups <-
        purrr::map(classes, \(concat_class) {
          data |>
            dplyr::filter(.data$gender %in% genders, .data$class %in% concat_class) |>
            dplyr::summarise(
              mean_score = quiet_means({{ var }}),
              denom = how_many_valid(valid_numbers({{ var }})),
              class = stringr::str_flatten(concat_class, collapse = ", ", last = " and "),
              .by = c("gender")
            )
        }) |>
        purrr::reduce(dplyr::bind_rows) |>
        dplyr::arrange(.data$class)
    }


    all <- data |>
      dplyr::mutate(class = "All", gender = dplyr::if_else(.gender_split, "All", "All pupils")) |>
      dplyr::summarise(
        mean_score = quiet_means({{ var }}),
        denom = how_many_valid(valid_numbers({{ var }})),
        .by = c("class", "gender")
      )

    dplyr::bind_rows(subgroups, all) |>
      dplyr::mutate(class = forcats::fct_inorder(class)) |>
      dplyr::arrange(.data$gender, .data$class)
  }

#' Bar graph of mean of a single var
#'
#' @param summary_data A dataframe produced by `summary_mean_single`
#' @param ymax Upper limit of graph (deaults to `limits[2]`)
#' @param ylab Label for X axis (summary statistic, i.e. "mean")
#'
#' @import ggplot2
#'
#' @returns A ggplot2 graph
bar_mean_single <- function(summary_data, ymax, ylab = "Mean") {
  summary_data |>
    dplyr::mutate(
      mean_score = dplyr::if_else(.data$censored, 1, .data$mean_score),
      bar_lab_main = dplyr::if_else(.data$censored, "*", sprintf("%.1f", .data$mean_score)),
      bar_lab_cens = dplyr::if_else(.data$censored, "Numbers too low to show", "")
    ) |>
    ggplot() +
    aes(
      x = .data$class,
      y = .data$mean_score,
      fill = .data$gender,
      linetype = .data$censored,
      alpha = .data$censored
    ) +
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
      size = dplyr::if_else(length(unique(summary_data$class)) > 3, 2.5, 4)
    ) +
    coord_cartesian(ylim = c(0, ymax), clip = "off") +
    labs(caption = dplyr::if_else(any(summary_data$censored), "* Numbers too low to show", ""))
}

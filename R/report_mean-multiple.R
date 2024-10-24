#' Table of means for multiple variables
#'
#'
#' @param data The dataframe of valid responses
#' @param varslist (named) list of variables to use. Names to match vars
#' @param genders List of genders to split by
#' @param classes List of classes to split by
#' @param .gender_split `TRUE`/`FALSE` - split by gender when sufficient numbers of responses
#'
#' @return A summary table of means of multiple variables
#'
#'
#' @import dplyr
#' @importFrom rlang .data
#'
summary_mean_multiple_vars <-
  function(data,
           varslist,
           genders = c("Boys", "Girls"),
           classes = "All",
           .gender_split = TRUE) {
    subgroups <- NULL

    if (.gender_split) {
      subgroups <-
        map(classes, \(concat_class) {
          class_summary <-
          data |>
            filter(gender %in% genders, class %in% concat_class) |>
            select(gender, class, !!!names(varslist)) |>
            mutate(class = str_flatten(concat_class, collapse = ", ", last = " and ")) |>
            summarise(across(everything(), list(
              mean = quiet_means, denominator = ~how_many_valid(valid_numbers(.x))
            ), .names = "{.col}__{.fn}"), .by = c("gender", "class")) |>
            arrange(gender)

          if (nrow(class_summary) == 0) {
            NULL
          } else {
            class_summary
          }
        })
    }

    all <- data |>
      mutate(class = "All", gender = if_else(.gender_split, "All", "All pupils")) |>
      select(gender, class, !!!names(varslist)) |>
      summarise(across(everything(), list(
        mean = quiet_means, denominator = ~how_many_valid(valid_numbers(.x))
      ), .names = "{.col}__{.fn}"), .by = c("gender", "class"))


    c(subgroups, list(all)) |>
      compact() |>
      map(\(class_data) {
        class_data |>
        tidyr::pivot_longer(-c(gender, class),
                            names_to = c("var", "x"),
                            names_sep = "__",
                            values_to = "n"
        ) |>
          tidyr::pivot_wider(names_from = x, values_from = n) |>
          rowwise() |>
          mutate(labels = stringr::str_wrap(varslist[[.data$var]][1], 12)) |>
          filter(!is.na(gender)) |>
          ungroup() |>
          mutate(labels = forcats::fct_reorder(.data$labels, mean))
      })
  }

#' Bar graph of means of multiple variables
#'
#' `bar_mean_multiple_vars` returns a horizontal bar graph.
#' `bar_mean_multiple_vertical` returns a vertical graph.
#'
#' @param summary_data Data produced by `summary_mean_multiple_vars`
#' @param xmax,ymax Upper limit of graph
#' @param xlab,ylab Label for X axis (summary statistic, i.e. "Mean")
#'

bar_mean_multiple_vars <- function(summary_data, xmax, xlab = "Mean") {
  class <- unique(summary_data$class)
  genders <- unique(summary_data$gender)

  summary_data |>
    mutate(
      mean = if_else(.data$censored, 1, .data$mean),
      bar_lab_main = if_else(.data$censored, "*", sprintf("%.1f", .data$mean)),
      bar_lab_cens = if_else(.data$censored, "Numbers too low to show", "")
    ) |>
    ggplot(
      aes(
        .data$mean,
        .data$labels,
        linetype = .data$censored,
        fill = .data$gender,
        colour = .data$gender,
        group = .data$gender
      )
    ) +
    geom_bar_t(aes(alpha = .data$censored),
      stat = "identity",
      width = 0.7,
      position = position_dodge(width = 0.7)
    ) +
    scale_alpha_manual(values = c("TRUE" = 0.6, "FALSE" = 1), guide = guide_none()) +
    scale_linetype_manual(
      values = c("TRUE" = "dashed", "FALSE" = "blank"),
      guide = guide_none()
    ) +
    scale_y_discrete("") +
    scale_fill_hbsc(
      aesthetics = c("fill", "colour"),
      name = "",
      limits = force
    ) +
    theme(
      legend.justification.right = "top",
      legend.title = element_blank(),
      legend.box.spacing = unit(c(0, 1.2, 0, 0), "cm"),
      plot.margin = unit(c(0.8, 1, 0.5, 0), "cm"),
      plot.caption = element_text(
        hjust = 1,
        size = 10,
        face = "italic"
      )
    ) +
    scale_x_continuous(xlab, expand = expansion(add = 0)) +
    geom_text(
      aes(label = .data$bar_lab_main),
      hjust = -0.3,
      colour = "black",
      position = position_dodge(width = 0.8),
      size = if_else(length(genders) > 1, 2.5, 3.5)
    ) +
    coord_cartesian(xlim = c(0, xmax), clip = "off") +
    labs(
      caption = if_else(any(summary_data$censored), "* Numbers too low to show", ""),
      title = paste(class, "pupils")
    )
}

#' @rdname bar_mean_multiple_vars
bar_mean_multiple_vertical <- function(summary_data, ymax, ylab = "Mean") {
  class <- unique(summary_data$class)
  varslist <- unique(summary_data$var)

  summary_data |>
    mutate(
      mean = if_else(.data$censored, 1, .data$mean),
      labels = forcats::fct_inorder(labels),
      bar_lab_main = if_else(.data$censored, "*", sprintf("%.1f", .data$mean)),
      bar_lab_cens = if_else(.data$censored, "Numbers too low to show", "")
    ) |>
    ggplot(
      aes(
        .data$labels,
        .data$mean,
        linetype = factor(.data$censored),
        fill = .data$gender,
        colour = .data$gender,
        group = .data$gender
      )
    ) +
    geom_bar_t(aes(alpha = .data$censored),
      stat = "identity",
      position = position_dodge(width = 0.7)
    ) +
    scale_alpha_manual(values = c("TRUE" = 0.6, "FALSE" = 1), guide = guide_none()) +
    scale_linetype_manual(
      values = c("TRUE" = "dashed", "FALSE" = "blank"),
      guide = guide_none()
    ) +
    scale_x_discrete("", guide = guide_axis(
      n.dodge =
        if_else(length(varslist) > 6,
          ceiling(length(varslist) / 4),
          1
        )
    )) +
    scale_fill_hbsc(
      aesthetics = c("fill", "colour"),
      name = "",
      limits = force
    ) +
    theme(
      legend.justification.right = "top",
      legend.title = element_blank(),
      plot.margin = unit(c(0.8, 1, 0.5, 0), "cm"),
      plot.title = element_text(margin = margin(0, 0, 20, 0)),
      plot.caption = element_text(
        hjust = 1,
        size = 10,
        face = "italic"
      )
    ) +
    scale_y_continuous(ylab, expand = expansion(add = 0)) +
    geom_text(
      aes(label = .data$bar_lab_main),
      vjust = -0.5,
      colour = "black",
      position = position_dodge(width = 0.7),
      size = if_else(length(class) > 3, 2, 3)
    ) +
    coord_cartesian(ylim = c(0, ymax), clip = "off") +
    labs(
      caption = if_else(any(summary_data$censored), "* Numbers too low to show", ""),
      title = paste(class, "pupils")
    )
}

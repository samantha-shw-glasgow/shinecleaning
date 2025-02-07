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
        purrr::map(classes, \(concat_class) {
          class_summary <-
          data |>
            dplyr::filter(.data$gender %in% genders, .data$class %in% concat_class) |>
            dplyr::select(.data$gender, .data$class, !!!names(varslist)) |>
            dplyr::mutate(class = stringr::str_flatten(concat_class, collapse = ", ", last = " and ")) |>
            dplyr::summarise(dplyr::across(dplyr::everything(), list(
              mean = quiet_means, denominator = ~how_many_valid(valid_numbers(.x))
            ), .names = "{.col}__{.fn}"), .by = c("gender", "class")) |>
            dplyr::arrange(.data$gender)

          if (nrow(class_summary) == 0) {
            NULL
          } else {
            class_summary
          }
        })
    }

    all <- data |>
      dplyr::mutate(class = "All", gender = dplyr::if_else(.gender_split, "All", "All pupils")) |>
      dplyr::select("gender", "class", !!!names(varslist)) |>
      dplyr::summarise(dplyr::across(dplyr::everything(), list(
        mean = quiet_means, denominator = ~how_many_valid(valid_numbers(.x))
      ), .names = "{.col}__{.fn}"), .by = c("gender", "class"))


    c(subgroups, list(all)) |>
      purrr::compact() |>
      purrr::map(\(class_data) {
        class_data |>
        tidyr::pivot_longer(-c("gender", "class"),
                            names_to = c("var", "x"),
                            names_sep = "__",
                            values_to = "n"
        ) |>
          tidyr::pivot_wider(names_from = "x", values_from = "n") |>
          dplyr::rowwise() |>
          dplyr::mutate(labels = stringr::str_wrap(varslist[[.data$var]][1], 12)) |>
          dplyr::filter(!is.na(.data$gender)) |>
          dplyr::ungroup() |>
          dplyr::mutate(labels = forcats::fct_reorder(.data$labels, mean))
      })
  }

#' Bar graph of means of multiple variables
#'
#' `bar_mean_multiple_vars` returns a horizontal bar graph.
#' `bar_mean_multiple_vertical` returns a vertical graph.
#'
#' @import ggplot2
#'
#' @param summary_data Data produced by `summary_mean_multiple_vars`
#' @param xmax,ymax Upper limit of graph
#' @param xlab,ylab Label for X axis (summary statistic, i.e. "Mean")
#'

bar_mean_multiple_vars <- function(summary_data, xmax, xlab = "Mean") {
  class <- unique(summary_data$class)
  genders <- unique(summary_data$gender)

  summary_data |>
    dplyr::mutate(
      mean = dplyr::if_else(.data$censored, 1, .data$mean),
      bar_lab_main = dplyr::if_else(.data$censored, "*", sprintf("%.1f", .data$mean)),
      bar_lab_cens = dplyr::if_else(.data$censored, "Numbers too low to show", "")
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        .data$mean,
        .data$labels,
        linetype = .data$censored,
        fill = .data$gender,
        colour = .data$gender,
        group = .data$gender
      )
    ) +
    geom_bar_t(ggplot2::aes(alpha = .data$censored),
      stat = "identity",
      width = 0.7,
      position = ggplot2::position_dodge(width = 0.7)
    ) +
    ggplot2::scale_alpha_manual(values = c("TRUE" = 0.6, "FALSE" = 1), guide = ggplot2::guide_none()) +
    ggplot2::scale_linetype_manual(
      values = c("TRUE" = "dashed", "FALSE" = "blank"),
      guide = ggplot2::guide_none()
    ) +
    ggplot2::scale_y_discrete("") +
    scale_fill_hbsc(
      aesthetics = c("fill", "colour"),
      name = "",
      limits = force
    ) +
    ggplot2::theme(
      legend.justification.right = "top",
      legend.title = ggplot2::element_blank(),
      legend.box.spacing = unit(c(0, 1.2, 0, 0), "cm"),
      plot.margin = unit(c(0.8, 1, 0.5, 0), "cm"),
      plot.caption = ggplot2::element_text(
        hjust = 1,
        size = 10,
        face = "italic"
      )
    ) +
    ggplot2::scale_x_continuous(xlab, expand = expansion(add = 0)) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$bar_lab_main),
      hjust = -0.3,
      colour = "black",
      position = ggplot2::position_dodge(width = 0.8),
      size = dplyr::if_else(length(genders) > 1, 2.5, 3.5)
    ) +
    coord_cartesian(xlim = c(0, xmax), clip = "off") +
    labs(
      caption = dplyr::if_else(any(summary_data$censored), "* Numbers too low to show", ""),
      title = paste(class, "pupils")
    )
}

#' @rdname bar_mean_multiple_vars
#' @import ggplot2
bar_mean_multiple_vertical <- function(summary_data, ymax, ylab = "Mean") {
  class <- unique(summary_data$class)
  varslist <- unique(summary_data$var)

  summary_data |>
    dplyr::mutate(
      mean = dplyr::if_else(.data$censored, 1, .data$mean),
      labels = forcats::fct_inorder(labels),
      bar_lab_main = dplyr::if_else(.data$censored, "*", sprintf("%.1f", .data$mean)),
      bar_lab_cens = dplyr::if_else(.data$censored, "Numbers too low to show", "")
    ) |>
    ggplot(
      ggplot2::aes(
        .data$labels,
        .data$mean,
        linetype = factor(.data$censored),
        fill = .data$gender,
        colour = .data$gender,
        group = .data$gender
      )
    ) +
    geom_bar_t(ggplot2::aes(alpha = .data$censored),
      stat = "identity",
      position = ggplot2::position_dodge(width = 0.7)
    ) +
    ggplot2::scale_alpha_manual(values = c("TRUE" = 0.6, "FALSE" = 1), guide = ggplot2::guide_none()) +
    ggplot2::scale_linetype_manual(
      values = c("TRUE" = "dashed", "FALSE" = "blank"),
      guide = ggplot2::guide_none()
    ) +
    ggplot2::scale_x_discrete("", guide = ggplot2::guide_axis(
      n.dodge =
        dplyr::if_else(length(varslist) > 6,
          ceiling(length(varslist) / 4),
          1
        )
    )) +
    scale_fill_hbsc(
      aesthetics = c("fill", "colour"),
      name = "",
      limits = force
    ) +
    ggplot2::theme(
      legend.justification.right = "top",
      legend.title = ggplot2::element_blank(),
      plot.margin = unit(c(0.8, 1, 0.5, 0), "cm"),
      plot.title = ggplot2::element_text(margin = margin(0, 0, 20, 0)),
      plot.caption = ggplot2::element_text(
        hjust = 1,
        size = 10,
        face = "italic"
      )
    ) +
    ggplot2::scale_y_continuous(ylab, expand = expansion(add = 0)) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$bar_lab_main),
      vjust = -0.5,
      colour = "black",
      position = ggplot2::position_dodge(width = 0.7),
      size = dplyr::if_else(length(class) > 3, 2, 3)
    ) +
    ggplot2::coord_cartesian(ylim = c(0, ymax), clip = "off") +
    ggplot2::labs(
      caption = dplyr::if_else(any(summary_data$censored), "* Numbers too low to show", ""),
      title = paste(class, "pupils")
    )
}

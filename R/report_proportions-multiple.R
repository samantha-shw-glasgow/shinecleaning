#' Table of % categories for multiple variables
#'
#' @param data The dataframe of valid responses
#' @param varslist (named) list of variables to use. Names to match vars
#' @param success A purrr-like function denoting numerator categories
#' @param genders List of genders to split by
#' @param classes List of classes to split by
#' @param .gender_split `TRUE`/`FALSE` - split by gender when sufficient numbers of responses
#'
#' @importFrom rlang .data
#'
#' @return A summary table of % of 'success' in each group
#'
summary_proportions_multiple <-
  function(data,
           varslist,
           success = ~ .x %in% c("More than once a week", "About every day"),
           genders = c("Boys", "Girls"),
           classes = "All",
           .gender_split = TRUE) {
    subgroups <- NULL

    if (.gender_split) {
      subgroups <-
        purrr::map(classes, \(concat_class) {
          class_summary <- data |>
            dplyr::filter(.data$gender %in% genders, .data$class %in% concat_class) |>
            dplyr::select("gender", "class", !!!names(varslist)) |>
            dplyr::mutate(class = stringr::str_flatten(concat_class, collapse = ", ", last = " and ")) |>
            dplyr::group_by(.data$gender, .data$class) |>
            dplyr::mutate(
              dplyr::across(dplyr::everything(), ~dplyr::na_if(.x, "Prefer not to say")),
              dplyr::across(dplyr::everything(), success),
              ) |>
            dplyr::summarise(dplyr::across(dplyr::everything(), list(numerator = ~sum(.x, na.rm = TRUE),
                                                denominator = how_many_valid),
                             .names = "{.col}__{.fn}"),
              .groups = "drop"
            ) |>
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
      dplyr::group_by(.data$gender, .data$class) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~dplyr::na_if(.x, "Prefer not to say")),
        dplyr::across(dplyr::everything(), success),
        ) |>
      dplyr::summarise(dplyr::across(dplyr::everything(), list(numerator = ~sum(.x, na.rm = TRUE),
                                          denominator = how_many_valid),
                       .names = "{.col}__{.fn}"),
                .groups = "drop"
      )

    c(subgroups, list(all)) |>
      purrr::compact() |>
      purrr::map(\(class_data) {
        class_data |>
          tidyr::pivot_longer(-c("gender", "class"),
            names_to = c("var", "x"),
            names_sep = "__",
            values_to = "numerator"
          ) |>
          tidyr::pivot_wider(names_from = "x", values_from = "numerator") |>
          dplyr::relocate("denominator", "var", "numerator", .after = dplyr::everything()) |>
          dplyr::rowwise() |>
          dplyr::mutate(
            labels = stringr::str_wrap(varslist[[.data$var]][1], 12),
            prop = .data$numerator / .data$denominator
          ) |>
          dplyr::filter(!is.na(.data$gender)) |>
          dplyr::ungroup() |>
          dplyr::mutate(labels = forcats::fct_reorder(.data$labels, .data$prop))
      })
  }

#' Bar proportions multiple
#'
#' @param summary_data Data produced by `summary_proportions_multiple`
#'
#' @import ggplot2
#'
#' @return A ggplot2 graph
bar_proportions_multiple <- function(summary_data) {
  class <- unique(summary_data$class)
  genders <- unique(summary_data$gender)

  summary_data |>
    dplyr::mutate(
      prop = dplyr::if_else(.data$censored, 0.01, .data$prop),
      bar_lab_main = dplyr::if_else(
        .data$censored,
        "*",
        scales::percent(.data$prop, suffix = "%", accuracy = 1)
      )
    ) |>
    ggplot(
      aes(
        .data$prop,
        .data$labels,
        linetype = .data$censored,
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
    scale_fill_hbsc(
      aesthetics = c("fill", "colour"),
      name = "",
      limits = force
    ) +
    scale_y_discrete("") +
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
    scale_x_continuous(
      "%",
      labels = scales::percent,
      limits = c(0, 1),
      expand = expansion(add = 0)
    ) +
    geom_text(
      aes(label = .data$bar_lab_main),
      hjust = -0.3,
      colour = "black",
      position = position_dodge(width = 0.8),
      size = dplyr::if_else(length(genders) > 1, 2.5, 3.5)
    ) +
    coord_cartesian(clip = "off") +
    labs(
      caption = dplyr::if_else(any(summary_data$censored), "* Numbers too low to show", ""),
      title = paste(class, "pupils")
    )
}

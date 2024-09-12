#' Table of % categories for multiple variables
#'
#' @param data The dataframe of valid responses
#' @param varslist (named) list of variables to use. Names to match vars
#' @param genders List of genders to split by
#' @param classes List of classes to split by
#' @param .censor `TRUE`/`FALSE` - apply censoring rules (must be `TRUE` in output reports)
#' @param .gender_split `TRUE`/`FALSE` - split by gender when sufficient numbers of responses
#'
#' @return A summary table of % of 'success' in each group
#'
summary_proportions_multiple <-
  function(data,
           varslist,
           success = ~ .x %in% c("More than once a week", "About every day"),
           genders = c("Boys", "Girls"),
           classes = "All",
           .censor = TRUE,
           .gender_split = TRUE
           ) {

    subgroups <- NULL

    if (.gender_split) {
      subgroups <-
        map(classes, \(concat_class) {
          data |>
            filter(gender %in% genders, class %in% concat_class) |>
            select(gender, class, !!!names(varslist)) |>
            mutate(class = str_flatten(concat_class, collapse = ", ", last = " and ")) |>
            group_by(gender, class) |>
            mutate(across(everything(), success)) |>
            summarise(across(everything(), sum), denom = n(),
                      .groups = "drop") |>
            arrange(gender)

        })
    }

    all <- data |>
      mutate(class = "All", gender = if_else(.gender_split, "All", "All pupils")) |>
      select(gender, class, !!!names(varslist)) |>
      group_by(gender, class) |>
      mutate(across(everything(), success)) |>
      summarise(across(everything(), sum), denom = n(),
                .groups = "drop")


    c(subgroups, list(all)) |>
      compact() |>
      map(\(class_data) {

        class_data |>
          tidyr::pivot_longer(-c(gender, class, denom),
                              names_to = "var",
                              values_to = "n") |>
          rowwise() |>
          mutate(
            censored = if_else(.data$n < 3 &
                                 .censor, 1, 0) |> factor(levels = c("1", "0")),
            labels = stringr::str_wrap(varslist[[.data$var]][1], 12),
            prop = .data$n / .data$denom,
            prop = if_else(.data$censored == 1, 0.05, .data$prop),
            bar_lab_main = if_else(
              .data$censored == 1,
              "*",
              scales::percent(.data$prop, suffix = "%", accuracy = 1)
            ),
            bar_lab_cens = if_else(.data$censored == 1, "Numbers too low to show", "")
          ) |>
          filter(!is.na(.data$gender)) |>
          ungroup() |>
          mutate(labels = forcats::fct_reorder(.data$labels, .data$prop))

      })

  }

#' Bar proportions multiple
#'
#' @param summary_data Data produced by `summary_proportions_multiple`
#'
#' @return A ggplot2 graph
bar_proportions_multiple <- function(summary_data) {

  class <- unique(summary_data$class)
  genders <- unique(summary_data$gender)

  summary_data |>
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
    geom_bar_t(aes(alpha = factor(.data$censored)),
               stat = "identity",
               position = position_dodge(width = 0.7)) +
    scale_alpha_manual(values = c("1" = 0.6, "0" = 1), guide = guide_none()) +
    scale_linetype_manual(values = c("1" = "dashed", "0" = "blank"),
                          guide = guide_none()) +
    scale_fill_hbsc(
      aesthetics = c("fill", "colour"),
      name = "",
      limits = force
    ) +
    scale_y_discrete("") +
    theme(
      legend.justification.right = "top",
      legend.title = element_blank(),
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
      hjust = -0.5,
      colour = "black",
      position = position_dodge(width = 0.8),
      size = if_else(length(genders) > 1, 2.5, 4)
    ) +
    coord_cartesian(clip = "off") +
    labs(
      caption = if_else(any(summary_data$censored == 1), "* Numbers too low to show", ""),
      title = paste(class, "pupils")
    )

}

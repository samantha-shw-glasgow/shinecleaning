#' Table of means for multiple variables
#'
#'
#' @param data The dataframe of valid responses
#' @param varslist (named) list of variables to use. Names to match vars
#' @param genders List of genders to split by
#' @param classes List of classes to split by
#' @param .censor `TRUE`/`FALSE` - apply censoring rules (must be `TRUE` in output reports)
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
            summarise(across(everything(), function(score) {
              mean(as.numeric(score), na.rm = TRUE)
            }), denom = n(), .by = c("gender", "class")) |>
            arrange(gender)

        })
    }

    all <- data |>
      mutate(class = "All", gender = if_else(.gender_split, "All", "All pupils")) |>
      select(gender, class, !!!names(varslist)) |>
      summarise(across(everything(), function(score) {
        mean(as.numeric(score), na.rm = TRUE)
      }), denom = n(), .by = c("gender", "class"))


    c(subgroups, list(all)) |>
      compact() |>
      map(\(class_data) {

     class_data |>
      tidyr::pivot_longer(-c(gender, class, denom),
                          names_to = "var",
                          values_to = "mean") |>
      rowwise() |>
      mutate(
        censored = if_else(.data$denom < 3 & .censor, 1, 0),
        mean = if_else(.data$censored == 1, 1, .data$mean),
        labels = stringr::str_wrap(varslist[[.data$var]][1], 12),
        bar_lab_main = if_else(.data$censored == 1, "*", sprintf("%.1f", .data$mean)),
        bar_lab_cens = if_else(.data$censored == 1, "Numbers too low to show", "")
      ) |>
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

  ggplot(
    summary_data,
    aes(
      .data$mean,
      .data$labels,
      linetype = factor(.data$censored),
      fill = .data$gender,
      colour = .data$gender,
      group = .data$gender
    )
  ) +
    geom_bar_t(aes(alpha = factor(.data$censored)),
               stat = "identity",
               width = 0.7,
               position = position_dodge(width = 0.7)) +
    scale_alpha_manual(values = c("1" = 0.6, "0" = 1), guide = guide_none()) +
    scale_linetype_manual(values = c("1" = "dashed", "0" = "blank"),
                          guide = guide_none()) +
    scale_y_discrete("") +
    scale_fill_hbsc(
      aesthetics = c("fill", "colour"),
      name = "",
      limits = force
    ) +
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
    scale_x_continuous(xlab, expand = expansion(add = 0)) +
    geom_text(
      aes(label = .data$bar_lab_main),
      hjust = -0.5,
      colour = "black",
      position = position_dodge(width = 0.8),
      size = if_else(length(genders) > 1, 2.5, 4)
    ) +
    coord_cartesian(xlim = c(0, xmax), clip = "off") +
    labs(
      caption = if_else(any(summary_data$censored == 1), "* Numbers too low to show", ""),
      title = paste(class, "pupils")
    )
}

#' @rdname bar_mean_multiple_vars
bar_mean_multiple_vertical <- function(summary_data, ymax, ylab = "Mean") {

  class <- unique(summary_data$class)
  varslist <- unique(summary_data$var)

  summary_data |>
    mutate(labels = forcats::fct_inorder(labels)) |>
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
    geom_bar_t(aes(alpha = factor(.data$censored)),
               stat = "identity",
               position = position_dodge(width = 0.7)) +
    scale_alpha_manual(values = c("1" = 0.6, "0" = 1), guide = guide_none()) +
    scale_linetype_manual(values = c("1" = "dashed", "0" = "blank"),
                          guide = guide_none()) +
    scale_x_discrete("", guide = guide_axis(n.dodge =
                                              if_else(length(varslist) > 6,
                                                      ceiling(length(varslist) / 4),
                                                      1)
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
      size = 4
    ) +
    coord_cartesian(ylim = c(0, ymax), clip = "off") +
    labs(
      caption = if_else(any(summary_data$censored == 1), "* Numbers too low to show", ""),
      title = paste(class, "pupils")
    )
}

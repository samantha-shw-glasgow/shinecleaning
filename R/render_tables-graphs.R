### This whole file should be split up and deprecated ###

#' Bar graph of means for multiple variables (horizontal)
#'
#' `bar_mean_multiple_vars` returns a horizontal bar graph.
#' `bar_mean_multiple_vertical` returns a vertical graph.
#'
#' @param data The dataframe of valid responses
#' @param varslist (named) list of variables to use. Names to match vars
#' @param group (Probably superceded) group bars by gender/class/none
#' @param .censor `TRUE`/`FALSE` - apply censoring rules (must be `TRUE` in output reports)
#' @param .gender_split `TRUE`/`FALSE` - split by gender when sufficient numbers of responses
#' @param limits Vector of upper/lower score limits.
#' @param xmax,ymax Upper limit of graph (deaults to `limits[2]`)
#' @param xlab,ylab Label for X axis (summary statistic, i.e. "mean")
#' @param classes Vector names of classes to filter/combine.
#'
#' @return A ggplot2 bar graph of means across several scoring variables
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang .data
#'
bar_mean_multiple_vars <-
  function(data,
           varslist,
           group = c("gender", "class", "none"),
           .censor = TRUE,
           .gender_split = TRUE,
           limits = c(`Poor quality` = 1, `High quality` = 6),
           xmax = limits[2],
           xlab = "Mean",
           classes = "All") {
    group <- match.arg(group)
    group <- if_else(.gender_split, group, "none")

    if (!("All" %in% classes)) {
      class_data <- data |>
        filter(class %in% classes)
    } else {
      class_data <- data
    }


    clean_dat <- class_data |>
      mutate(
        grouping = case_when(
          group == "none" ~ "All pupils",
          group == "gender" ~ as.character(.data$gender),
          group == "class" ~ as.character(.data$class)
        )
      ) |>
      select(.data$grouping, !!!names(varslist)) |>
      filter(if_all(everything(), .fns = ~ !is.na(.x))) |>
      group_by(.data$grouping) |>
      mutate(across(everything(), function(score) {
        chr_score <-  as.character(score)
        if_else(chr_score %in% names(limits),
                unname(limits[chr_score]),
                as.numeric(chr_score))
      })) |>
      summarise(across(everything(), function(score) {
        mean(score, na.rm = TRUE)
      }), denom = n()) |>
      tidyr::pivot_longer(-c(.data$grouping, .data$denom),
                   names_to = "var",
                   values_to = "mean") |>
      rowwise() |>
      mutate(
        censored = if_else(.data$denom < 3 & .censor, 1, 0),
        mean = if_else(.data$censored == 1, xmax / 20, .data$mean),
        labels = stringr::str_wrap(varslist[[.data$var]][1], 12),
        bar_lab_main = if_else(.data$censored == 1, "*", sprintf("%.1f", .data$mean)),
        bar_lab_cens = if_else(.data$censored == 1, "Numbers too low to show", ""),
        grouping = factor(
          .data$grouping,
          levels = c("Girls", "Boys", "S2", "S4", "All pupils")
        )
      ) |>
      filter(!is.na(grouping)) |>
      ungroup() |>
      mutate(labels = forcats::fct_reorder(.data$labels, mean))

    ggplot(
      clean_dat,
      aes(
        .data$mean,
        .data$labels,
        linetype = factor(.data$censored),
        fill = .data$grouping,
        colour = .data$grouping,
        group = .data$grouping
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
        size = if_else(.gender_split, 2.5, 4)
      ) +
      # geom_text(aes(label = bar_lab_cens, y = ymax/2),
      #           # nudge_y = 0.05,
      #           vjust = 0.5,
      #           angle = 90,
      #           colour = "black",
      #           position = position_dodge(width = 0.7),
      #           size = if_else(.gender_split, 3, 4)) +
      coord_cartesian(xlim = c(0, xmax), clip = "off") +
      labs(
        caption = if_else(any(clean_dat$censored == 1), "* Numbers too low to show", ""),
        title = paste(stringr::str_flatten_comma(classes, " and "), "pupils")
      )
  }

#' Bar graph of means for a single variable
#'
#' @param data Valid input data
#' @param var Variable to calculate mean of
#' @param .censor `TRUE`/`FALSE` - apply censoring rules (must be `TRUE` in output reports)
#' @param .gender_split `TRUE`/`FALSE` - split by gender when sufficient numbers of responses
#' @param limits Vector of upper/lower score limits.
#' @param ymax Upper limit of graph (deaults to `limits[2]`)
#' @param ylab Label for X axis (summary statistic, i.e. "mean")
#' @param classes Vector names of classes to filter/combine.
#'
#' @return A ggplot2 bar graph of mean of a scoring variables
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang .data
#'
bar_mean_single_var <-
  function(data,
           var,
           .censor = TRUE,
           .gender_split = TRUE,
           limits = c(`Poor quality` = 1, `High quality` = 6),
           ymax = limits[2],
           ylab = "Mean",
           classes = "All") {
    subgroups <- tibble()

    if (.gender_split) {
      subgroups <-
        map(classes, \(concat_class) {
          data |>
            # Can this filter for two sets of groupings - class concat or expand?
            filter(gender %in% c("Boys", "Girls"), class %in% concat_class) |>
            summarise(
              mean_score = mean({{var}}, na.rm = TRUE),
              class = str_flatten(concat_class, collapse = ", ", last = " and "),
              .by = c(gender)
            )
        }) |>
        reduce(bind_rows) |>
        arrange(class)
    }


    all <- data |>
      mutate(class = "All", gender = if_else(.gender_split, "All", "All pupils")) |>
      summarise(mean_score = mean({{var}}, na.rm = TRUE),
                .by = c(class, gender))

    bind_rows(subgroups, all) |>
      mutate(bar_lab_main = sprintf("%.1f", mean_score),
             class = fct_inorder(class)) |>
      ggplot() +
      aes(x = class, y = mean_score, fill = gender) +
      # geom_col(position = "dodge") +
      geom_bar_t(stat = "identity",
                 position = position_dodge(width = 0.7),
                 linetype = "blank") +
      scale_x_discrete("") +
      scale_fill_hbsc(name = "") +
      theme(legend.justification.right = "top",
            legend.title = element_blank(),
            plot.margin = unit(c(0.8, 1, 0.5, 0), "cm")) +
      scale_y_continuous(ylab, expand = expansion(add = 0)) +
      geom_text(
        aes(label = .data$bar_lab_main),
        vjust = -0.5,
        colour = "black",
        position = position_dodge(width = 0.7),
        size = 4
      ) +
      coord_cartesian(ylim = c(0, ymax), clip = "off")

  }

#' Bar graph of % categories for multiple variables
#'
#' @param data The dataframe of valid responses
#' @param varslist (named) list of variables to use. Names will provide graph labels
#' @param success A purrr-like formula to determine observation in 'inclusion' group
#' @param group (Probably superseded) group bars by gender/class/none
#' @param .censor `TRUE`/`FALSE` - apply censoring rules (must be `TRUE` in output reports)
#' @param .gender_split `TRUE`/`FALSE` - split by gender when sufficient numbers of responses
#' @param classes Vector names of classes to filter/combine.
#'
#' @return A ggplot2 graph of % of 'success' in each group
#'
bar_multiple_vars <-
  function(data,
           varslist,
           success = ~ .x %in% c("More than once a week", "About every day"),
           group = c("gender", "class", "none"),
           .censor = TRUE,
           .gender_split = TRUE,
           # This currently overrides `group`
           classes = "All") {
    group <- match.arg(group)

    group <- if_else(.gender_split, group, "none")

    if (!("All" %in% classes)) {
      class_data <- data |>
        filter(class %in% classes)
    } else {
      class_data <- data
    }

    clean_dat <- class_data |>
      mutate(
        grouping = case_when(
          group == "none" ~ "All pupils",
          group == "gender" ~ as.character(gender),
          group == "class" ~ as.character(class)
        )
      ) |>
      group_by(.data$grouping) |>
      select(.data$grouping, !!!names(varslist)) |>
      mutate(across(everything(), success)) |>
      summarise(across(everything(), sum), denom = n()) |>
      tidyr::pivot_longer(-c(.data$grouping, .data$denom),
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
        bar_lab_cens = if_else(.data$censored == 1, "Numbers too low to show", ""),
        grouping = factor(.data$grouping, levels = c("Girls", "Boys", "S2", "S4", "All pupils"))
      ) |>
      filter(!is.na(.data$grouping)) |>
      ungroup() |>
      mutate(labels = forcats::fct_reorder(.data$labels, .data$prop))

    clean_dat |>
      ggplot(
        aes(
          .data$prop,
          .data$labels,
          linetype = .data$censored,
          fill = .data$grouping,
          colour = .data$grouping,
          group = .data$grouping
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
        size = if_else(.gender_split, 2.5, 4)
      ) +
      coord_cartesian(clip = "off") +
      labs(
        caption = if_else(any(clean_dat$censored == 1), "* Numbers too low to show", ""),
        title = paste(stringr::str_flatten_comma(classes, " and "), "pupils")
      )

  }


#' @rdname bar_mean_multiple_vars
bar_mean_multiple_vertical <-
  function(data,
           varslist,
           group = c("gender", "class", "none"),
           .censor = TRUE,
           .gender_split = TRUE,
           limits = c(`Poor quality` = 1, `High quality` = 6),
           ymax = limits[2],
           ylab = "Mean",
           classes = "All") {

    group <- match.arg(group)
    group <- if_else(.gender_split, group, "none")

    if (!("All" %in% classes)) {
      class_data <- data |>
        filter(class %in% classes)
    } else {
      class_data <- data
    }

    clean_dat <- class_data |>
      mutate(
        grouping = case_when(
          group == "none" ~ "All pupils",
          group == "gender" ~ as.character(.data$gender),
          group == "class" ~ as.character(.data$class)
        )
      ) |>
      select(.data$grouping, !!!names(varslist)) |>
      filter(if_all(everything(), .fns = ~ !is.na(.x))) |>
      group_by(.data$grouping) |>
      mutate(across(everything(), function(score) {
        chr_score <-  as.character(score)
        if_else(chr_score %in% names(limits),
                unname(limits[chr_score]),
                as.numeric(chr_score))
      })) |>
      summarise(across(everything(), function(score) {
        mean(score, na.rm = TRUE)
      }), denom = n()) |>
      tidyr::pivot_longer(-c(.data$grouping, .data$denom),
                          names_to = "var",
                          values_to = "mean") |>
      rowwise() |>
      mutate(
        censored = if_else(.data$denom < 3 & .censor, 1, 0),
        mean = if_else(.data$censored == 1, ymax / 20, .data$mean),
        labels = stringr::str_wrap(varslist[[.data$var]][1], 12),
        bar_lab_main = if_else(.data$censored == 1, "*", sprintf("%.1f", .data$mean)),
        bar_lab_cens = if_else(.data$censored == 1, "Numbers too low to show", ""),
        grouping = factor(
          .data$grouping,
          levels = c("Girls", "Boys", "S2", "S4", "All pupils")
        )
      ) |>
      filter(!is.na(grouping)) |>
      ungroup() |>
      mutate(labels = factor(.data$labels, levels = varslist))

    ggplot(
      clean_dat,
      aes(
        .data$labels,
        .data$mean,
        linetype = factor(.data$censored),
        fill = .data$grouping,
        colour = .data$grouping,
        group = .data$grouping
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
        caption = if_else(any(clean_dat$censored == 1), "* Numbers too low to show", ""),
        title = paste(stringr::str_flatten_comma(classes, " and "), "pupils")
      )
  }

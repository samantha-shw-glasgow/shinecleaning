#' Generate text to describe input date range
#'
#' @param input_data The dataframe of survey responses with the column `StartDate`
#'
#' @return A string describing month of survey (or range)
#'
date_range <- function(input_data) {
  start <- min(lubridate::ymd_hms(input_data$`StartDate`))
  end <- max(lubridate::ymd_hms(input_data$`StartDate`))

  start_date <- paste(lubridate::month(start, abbr = FALSE, label = TRUE),
                      lubridate::year(start))
  end_date <- paste(lubridate::month(end, abbr = FALSE, label = TRUE),
                    lubridate::year(end))

  if (start_date == end_date) {
    paste("in", start_date)
  } else {
    paste("from", start_date, "to", end_date)
  }

}


#' Generate table reporting survey response rate
#'
#' @param input_data The dataframe of valid responses
#' @param n_invited Number invited to complete survey
#'
#' @return A table (markdown) detailing response rate
#'
#' @import flextable
#'
tab_responses <- function(input_data, n_invited) {

  n_responses <- nrow(input_data)
  n_missing <- n_invited - n_responses
  perc_rate <- 100 * n_responses / n_invited

  tibble::tribble(
    ~a, ~b,
    "Number of pupils invited to participate", n_invited |> as.character(),
    "Number of pupils who did not take part", n_missing |> as.character(),
    "Overall response rate", sprintf("%.0f%%", perc_rate)
  ) |>
    flextable() |>
    delete_part("header") |>
    theme_vanilla() |>
    # border_inner_h(officer::fp_border("gray", width = 1)) |>
    set_table_properties(layout = "autofit", width = 1) |>
    set_caption("Response rate", align_with_table = FALSE)

}


#' Render bar percentage
#'
#' @param data The dataframe of valid responses
#' @param var Variable to use
#' @param success Value(s) in `var` denoting 'success' (i.e. count as percentage)
#' @param .censor `TRUE`/`FALSE` - apply censoring rules (must be `TRUE` in output reports)
#' @param .gender_split `TRUE`/`FALSE` - split by gender when sufficient numbers of responses
#'
#' @return A ggplot2 graph - percentage of successes in a single category.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang .data
#'
bar_by_cat <- function(data,
                       var,
                       success = "Yes",
                       .censor = TRUE,
                       .gender_split = TRUE) {

  require(patchwork)

  var <- enquo(var)

  df_gender <- data |>
    group_by(.data$gender) |>
    mutate(success = {{var}} %in% success) |>
    summarise(numerator = sum(success, na.rm = TRUE),
              denom = n()) |>
    filter(!is.na(.data$gender))

  if (((all(df_gender$numerator > 3) &&
          all(df_gender$denom >= 7)) || !.censor) && .gender_split) {
    # * chart should not be created if there are ≤3 students in the numerator of
    # any variable.
    # * only separate by gender if there are ≥7 girls AND ≥7 boys in the
    # denominator of any variable

    p1 <- df_gender |>
      mutate(prop = .data$numerator / .data$denom) |>
      ggplot(aes(.data$gender, .data$prop, fill = .data$gender)) +
      geom_bar(stat = "identity") +
      scale_fill_hbsc("") +
      xlab("") +
      scale_y_continuous("%", labels = scales::percent) +
      geom_text(
        aes(label = scales::percent(
          .data$prop, suffix = "%", accuracy = 1
        )),
        vjust = 0,
        nudge_y = 0.05,
        size = 4
      ) +
      theme(plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm")) +
      coord_cartesian(ylim = c(0, 1), clip = "off")

  } else {
    # Test semi-censored version

    # } else if (all(df_gender$numerator > 3) & sum(df_gender$denom <= 14)) {
    # * if there are ≤14 students, the chart should only present a single column
    #   representing all students.

    p1 <- data |>
      mutate(success = !!var %in% success) |>
      summarise(prop = sum(success, na.rm = TRUE) / n()) |>
      mutate(gender = "All pupils") |>
      ggplot(aes(.data$gender, .data$prop, fill = .data$gender)) +
      geom_bar(stat = "identity") +
      scale_fill_hbsc("") +
      xlab("") +
      scale_y_continuous("%", labels = scales::percent) +
      geom_text(
        aes(label = scales::percent(
          .data$prop, suffix = "%", accuracy = 1
        )),
        vjust = 0,
        nudge_y = 0.05,
        size = 4
      ) +
      theme(plot.margin = unit(c(0.8, 0.5, 0.5, 1), "cm")) +
      coord_cartesian(ylim = c(0, 1), clip = "off")
  }

  df_school <- data |>
    group_by(.data$class) |>
    mutate(success = !!var %in% success) |>
    summarise(
      numerator = sum(.data$success),
      denom = n(),
      .groups = "keep"
    ) |>
    filter(!is.na(.data$class))

  if (((
    length(df_school$class) == 2 &&
    all(df_gender$numerator > 3) && all(df_gender$denom >= 7) &&
    all(df_school$denom >= 7)
  ) || (length(df_school$class) == 2 && .censor == FALSE))) {
    # * for secondary schools, only separate if there are ≥7 S2 AND ≥7 S4).

    p2 <- df_school |>
      summarise(prop = sum(.data$numerator) / sum(.data$denom)) |>
      ggplot(aes(.data$class, .data$prop, fill = .data$class)) +
      geom_bar(stat = "identity") +
      scale_fill_hbsc("") +
      xlab("") +
      scale_y_continuous("", labels = NULL, position = "right") +
      theme(
        axis.ticks.y = element_line(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        plot.margin = unit(c(0.8, 0.5, 0.5, 1), "cm")
      ) +
      geom_text(
        aes(label = scales::percent(
          .data$prop, suffix = "%", accuracy = 1
        )),
        vjust = 0,
        nudge_y = 0.05,
        size = 4
      ) +
      coord_cartesian(ylim = c(0, 1), clip = "off")
  } else {
    p2 <- NULL
  }

  p1 + p2
}

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
        position = position_dodge(width = 0.7),
        size = 4
      ) +
      # geom_text(aes(label = bar_lab_cens, y = ymax/2),
      #           # nudge_y = 0.05,
      #           vjust = 0.5,
      #           angle = 90,
      #           colour = "black",
      #           position = position_dodge(width = 0.7),
      #           size = 4) +
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
      mutate(class = "All", gender = "All") |>
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
#' @param success A purr-like formula to determine observation in 'inclusion' group
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
        position = position_dodge(width = 0.7),
        size = 4
      ) +
      coord_cartesian(clip = "off") +
      labs(
        caption = if_else(any(clean_dat$censored == 1), "* Numbers too low to show", ""),
        title = paste(stringr::str_flatten_comma(classes, " and "), "pupils")
      )

  }


#' Standard categorical fill scale
#'
#' @param ... Other arguments passed to `scale_fill_manual`
#'
scale_fill_hbsc <- function(...) {

  primary_colour <-  "#4770b7"
  # secondary_colour <- "#016bb2"
  # main_colour <- "#333333"
  global_all_pupils_colour <- "#37474f"
  global_girls_colour <- "#88cbec"
  global_boys_colour <- "#4d648d"
  global_s2_colour <- "#548235"
  global_s4_colour <- "#C5E0B4"
  global_expected_colour <- "#4770b7"
  global_elevated_colour <- "#ee9457"

  ggplot2::scale_fill_manual(
    values = c(
      "Girls" = global_girls_colour,
      "Boys" = global_boys_colour,
      "S2" = global_s2_colour,
      "S4" = global_s4_colour,
      "All pupils" = primary_colour,
      "All" = global_all_pupils_colour,
      "Elevated" = global_elevated_colour,
      "As expected" = global_expected_colour,
      "1" = primary_colour
    ),
    ...
  )


}

#' Thinner geom_bar
#'
#' @param width Width of bar (default 0.5)
#' @param ... Other arguments to pass to `geom_bar`

geom_bar_t <- function(..., width = 0.7) {
  geom_bar(..., width = width)
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

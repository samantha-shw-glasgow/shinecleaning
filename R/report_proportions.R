#' Collapsed summary (percentages of successes) for proportion graphs
#'
#' @param data Valid input data
#' @param var Variable to calculate by
#' @param success Character vector of categories as 'successes'
#' @param genders List of genders to split by
#' @param classes List of classes to split by
#' @param .gender_split Gender split - passed from params
#'
#' @return A dataframe of proportions/counts of successes
create_collapsed_summary <- function(
    data,
    var,
    success,
    genders,
    classes,
    .gender_split = FALSE) {

  subgroups <-
    map(classes, \(concat_class) {
      data |>
        filter(class %in% concat_class, gender %in% genders) |>
        filter({{var}} != "Prefer not to say", !is.na({{var}})) |>
        group_by(gender, class) |>
        mutate(
          success = {{ var }} %in% success,
          class = str_flatten(concat_class, collapse = ", ", last = " and ")
          ) |>
        summarise(
          numerator = sum(success, na.rm = TRUE),
          denominator = n(),
          .groups = "drop"
        )
    }) |>
  reduce(bind_rows) |>
    arrange(class)

  all <- data |>
    filter({{var}} != "Prefer not to say", !is.na({{var}})) |>
    mutate(class = "All", gender = "All") |>
    group_by(gender, class) |>
    mutate(
      success = {{ var }} %in% success
    ) |>
    summarise(
      numerator = sum(success, na.rm = TRUE),
      denominator = n(),
      .groups = "drop"
    )

  if (.gender_split) {
    joined_dat <- subgroups |>
      filter(gender %in% genders) |>
      bind_rows(all)
  } else {
    joined_dat <- all |>
      mutate(gender = "All pupils")
  }

  joined_dat |>
    mutate(class = forcats::fct_inorder(class)) |>
    relocate(gender, .after = class)
}

#' Full summary counts across all categories
#'
#' @param data Valid input data
#' @param var Variable to calculate by
#' @param levels Character vector of ordered levels
#' @param genders List of genders to split by
#' @param classes List of classes to split by
#' @param .gender_split Gender split - passed from params
#'
#' @import ggplot2
#'
#' @return A dataframe of counted variables
create_full_summary <- function(
    data,
    var,
    levels,
    genders,
    classes,
    .gender_split = FALSE) {
  var <- enquo(var)

    subgroups <-
    map(classes, \(concat_class) {
      data |>
        rename(answer = !!var) |>
        filter(answer %in% levels) |>
        filter(class %in% concat_class) |>
        mutate(class = str_flatten(concat_class, collapse = ", ", last = " and ")) |>
        mutate(across(c(gender, answer, class), factor)) |>
        group_by(gender, answer, class, .drop = FALSE) |>
        summarise(numerator = n(), .groups = "drop") |>
        add_count(across(c(gender, class)), name = "denominator", wt = numerator)
    }) |>
    reduce(bind_rows) |>
    arrange(class)

    all <- data |>
      rename(answer = !!var) |>
      filter(answer %in% levels) |>
      summarise(numerator = n(), .by = "answer") |>
      add_count(name = "denominator", wt = numerator) |>
      mutate(class = "All", gender = "All")

  if (.gender_split) {
    joined_dat <- subgroups |>
      filter(gender %in% genders) |>
      bind_rows(all)
  } else {
    joined_dat <- all
  }

  joined_dat |>
    transmute(
      class = forcats::fct_inorder(class),
      gender = as.character(gender),
      answer = factor(answer, levels = levels),
      numerator,
      denominator
    ) |>
    arrange(class, gender, desc(answer))
}


#' Bar percentage from summary data
#'
#' @param summary_data A dataframe produced by `create_collapsed_summary`
#' @param hbsc_data A dataframe with national data to add as points
#'
#' @return A ggplot2 graph
bar_from_summary <- function(summary_data, hbsc_data = NULL) {
  hbsc_data_in <- tibble(
    gender = character(),
    class = factor(levels = levels(summary_data$class)),
    hbsc_gender = character(),
    hbsc_class = character()
  )

  if (!is.null(hbsc_data)) {
    hbsc_data_in <-
      hbsc_data |>
      mutate(
        hbsc_prop = prop,
        hbsc_gender = gender,
        gender = str_extract(gender, "^\\w*"),
        class = map_chr(
          class,
          ~levels(summary_data$class)[which.max(
            stringr::str_detect(levels(summary_data$class), .x)
            )]
        ) |>
          factor(levels = levels(summary_data$class))
      ) |>
      select(class, gender, hbsc_gender, hbsc_prop) |>
      unique()
  }

  included_genders <- inner_join(summary_data, hbsc_data_in, by = join_by(class, gender)) |>
    pull(hbsc_gender) |>
    unique()

  hbsc_data_colours <- list("Boys (Scotland)" = "#fb1e20", "Girls (Scotland)" = "#008000")[included_genders]

  summary_data |>
    mutate(
      prop = if_else(.data$censored, 0.05, numerator / denominator),
      bar_lab_main = if_else(
        .data$censored,
        "*",
        scales::percent(.data$prop, suffix = "%", accuracy = 1)
      )
    ) |>
    left_join(hbsc_data_in, by = join_by(class, gender)) |>
    ggplot(aes(
      x = .data$class,
      y = .data$prop,
      fill = .data$gender,
      linetype = .data$censored,
      alpha = .data$censored,
      shape = .data$hbsc_gender
    )) +
    geom_bar(position = "dodge", stat = "identity") +
    {
      if (!is.null(hbsc_data)) {
        geom_point(
          aes(
            y = hbsc_prop,
            fill = hbsc_gender,
            colour = hbsc_gender
          ),
          position = position_dodge(0.9),
          size = 2
        )
      }
    } +
    scale_shape_manual(
      values = c("Boys (Scotland)" = 21, "Girls (Scotland)" = 24),
      na.translate = FALSE,
      guide = guide_legend(
        order = 2,
        override.aes = list(
          fill = hbsc_data_colours,
          colour = rep(NA, length(hbsc_data_colours))
        )
      )
    ) +
    scale_alpha_manual(values = c("TRUE" = 0.6, "FALSE" = 1), guide = guide_none()) +
    scale_linetype_manual(
      values = c("TRUE" = "dashed", "FALSE" = "blank"),
      guide = guide_none()
    ) +
    scale_fill_hbsc(
      "",
      aesthetics = c("colour", "fill"),
      breaks = c("All pupils", "All", "Boys", "Girls"),
      na.translate = FALSE,
      guide = guide_legend(order = 1)
    ) +
    xlab("") +
    scale_y_continuous("%", labels = scales::percent, expand = expansion()) +
    geom_text(
      aes(label = .data$bar_lab_main),
      color = "black",
      position = position_dodge(0.9),
      vjust = -0.5,
      size = if_else(length(unique(summary_data$class)) > 3, 2.5, 4)
    ) +
    theme(
      plot.margin = unit(c(0.8, 0.5, 0.5, 1), "cm"),
      legend.title = element_blank(),
      legend.key = element_blank(),
      legend.box.background = element_blank()
    ) +
    coord_cartesian(ylim = c(0, 1), clip = "off") +
    labs(caption = if_else(any(summary_data$censored == 1), "* Numbers too low to show", ""))
}

#' A table of percentages from summary data
#'
#' @param summary_data A dataframe produced by `create_collapsed_summary`
#'
#' @return A printed `flextable`
#'
table_from_summary <- function(summary_data) {
  tab <- summary_data |>
    mutate(prop = if_else(
      censored,
      "*",
      sprintf("%.0f", 100 * numerator / denominator))
    ) |>
    pivot_wider(id_cols = answer, names_from = c(class, gender), values_from = prop) |>
    rename(`All\n%` = All_All, ` ` = answer) |>
    rename_with(~ str_replace(.x, "(\\d)(?=_)", "\\1\n%")) |>
    flextable() |>
    separate_header() |>
    theme_vanilla() |>
    set_table_properties(layout = "autofit", width = 1) |>
    keep_with_next() |>
    set_caption(align_with_table = FALSE) |>
    fontsize(size = 9, part = "all") |>
    align(j = -1, align = "center", part = "all")
  if (any(summary_data$censored)) {
    tab |>
      add_footer_lines("* Numbers too low to show")
  } else {
    tab
  }
}

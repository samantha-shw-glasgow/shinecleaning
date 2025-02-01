#' Collapsed summary (percentages of successes) for proportion graphs
#'
#' @param data Valid input data
#' @param var Variable to calculate by
#' @param success Character vector of categories as 'successes'
#' @param genders List of genders to split by
#' @param classes List of classes to split by
#' @param .gender_split Gender split - passed from params
#'
#' @import rlang
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
    purrr::map(classes, \(concat_class) {
      data |>
        dplyr::filter(.data$class %in% concat_class, .data$gender %in% genders) |>
        dplyr::filter({{var}} != "Prefer not to say", !is.na({{var}})) |>
        dplyr::group_by(.data$gender, .data$class) |>
        dplyr::mutate(
          success = {{ var }} %in% success,
          class = stringr::str_flatten(concat_class, collapse = ", ", last = " and ")
          ) |>
        dplyr::summarise(
          numerator = sum(success, na.rm = TRUE),
          denominator = dplyr::n(),
          .groups = "drop"
        )
    }) |>
  purrr::reduce(dplyr::bind_rows) |>
    dplyr::arrange(class)

  all <- data |>
    dplyr::filter({{var}} != "Prefer not to say", !is.na({{var}})) |>
    dplyr::mutate(class = "All", gender = "All") |>
    dplyr::group_by(.data$gender, .data$class) |>
    dplyr::mutate(
      success = {{ var }} %in% success
    ) |>
    dplyr::summarise(
      numerator = sum(success, na.rm = TRUE),
      denominator = dplyr::n(),
      .groups = "drop"
    )

  if (.gender_split) {
    joined_dat <- subgroups |>
      dplyr::filter(.data$gender %in% genders) |>
      dplyr::bind_rows(all)
  } else {
    joined_dat <- all |>
      dplyr::mutate(gender = "All pupils")
  }

  joined_dat |>
    dplyr::mutate(class = forcats::fct_inorder(.data$class)) |>
    dplyr::relocate("gender", .after = class)
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
#'
#' @return A dataframe of counted variables
create_full_summary <- function(
    data,
    var,
    levels,
    genders,
    classes,
    .gender_split = FALSE) {
  var <- rlang::enquo(var)

    subgroups <-
    purrr::map(classes, \(concat_class) {
      data |>
        dplyr::rename(answer = !!var) |>
        dplyr::filter(.data$answer %in% levels) |>
        dplyr::filter(class %in% concat_class) |>
        dplyr::mutate(class = stringr::str_flatten(concat_class, collapse = ", ", last = " and ")) |>
        dplyr::mutate(dplyr::across(c("gender", "answer", "class"), factor)) |>
        dplyr::group_by(.data$gender, .data$answer, .data$class, .drop = FALSE) |>
        dplyr::summarise(numerator = dplyr::n(), .groups = "drop") |>
        dplyr::add_count(dplyr::across(c("gender", "class")), name = "denominator", wt = .data$numerator)
    }) |>
    purrr::reduce(dplyr::bind_rows) |>
    dplyr::arrange(class)

    all <- data |>
      dplyr::rename(answer = !!var) |>
      dplyr::filter(.data$answer %in% levels) |>
      dplyr::summarise(numerator = dplyr::n(), .by = "answer") |>
      dplyr::add_count(name = "denominator", wt = .data$numerator) |>
      dplyr::mutate(class = "All", gender = "All")

  if (.gender_split) {
    joined_dat <- subgroups |>
      dplyr::filter(.data$gender %in% genders) |>
      dplyr::bind_rows(all)
  } else {
    joined_dat <- all
  }

  joined_dat |>
    dplyr::transmute(
      class = forcats::fct_inorder(class),
      gender = as.character(.data$gender),
      answer = factor(.data$answer, levels = levels),
      .data$numerator,
      .data$denominator
    ) |>
    dplyr::arrange(.data$class, .data$gender, dplyr::desc(.data$answer))
}


#' Bar percentage from summary data
#'
#' @param summary_data A dataframe produced by `create_collapsed_summary`
#' @param hbsc_data A dataframe with national data to add as points
#'
#' @import ggplot2
#'
#' @return A ggplot2 graph
bar_from_summary <- function(summary_data, hbsc_data = NULL) {
  hbsc_data_in <- dplyr::tibble(
    gender = character(),
    class = factor(levels = levels(summary_data$class)),
    hbsc_gender = character(),
    hbsc_class = character()
  )

  if (!is.null(hbsc_data)) {
    hbsc_data_in <-
      hbsc_data |>
      dplyr::mutate(
        hbsc_prop = .data$prop,
        hbsc_gender = .data$gender,
        gender = stringr::str_extract(.data$gender, "^\\w*"),
        class = purrr::map_chr(
          class,
          ~levels(summary_data$class)[which.max(
            stringr::str_detect(levels(summary_data$class), .x)
            )]
        ) |>
          factor(levels = levels(summary_data$class))
      ) |>
      dplyr::select("class", "gender", "hbsc_gender", "hbsc_prop") |>
      unique()
  }

  included_genders <- dplyr::inner_join(summary_data, hbsc_data_in, by = dplyr::join_by("class", "gender")) |>
    dplyr::pull(.data$hbsc_gender) |>
    unique()

  hbsc_data_colours <- list("Boys (Scotland)" = "#fb1e20", "Girls (Scotland)" = "#008000")[included_genders]

  summary_data |>
    dplyr::mutate(
      prop = dplyr::if_else(.data$censored, 0.05, .data$numerator / .data$denominator),
      bar_lab_main = dplyr::if_else(
        .data$censored,
        "*",
        scales::percent(.data$prop, suffix = "%", accuracy = 1)
      )
    ) |>
    dplyr::left_join(hbsc_data_in, by = dplyr::join_by("class", "gender")) |>
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
            y = .data$hbsc_prop,
            fill = .data$hbsc_gender,
            colour = .data$hbsc_gender
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
      size = dplyr::if_else(length(unique(summary_data$class)) > 3, 2.5, 4)
    ) +
    theme(
      plot.margin = unit(c(0.8, 0.5, 0.5, 1), "cm"),
      legend.title = element_blank(),
      legend.key = element_blank(),
      legend.box.background = element_blank()
    ) +
    coord_cartesian(ylim = c(0, 1), clip = "off") +
    labs(caption = dplyr::if_else(any(summary_data$censored == 1), "* Numbers too low to show", ""))
}

#' A table of percentages from summary data
#'
#' @param summary_data A dataframe produced by `create_collapsed_summary`
#'
#' @return A printed `flextable`
#'
table_from_summary <- function(summary_data) {
  tab <- summary_data |>
    dplyr::mutate(prop = dplyr::if_else(
      .data$censored,
      "*",
      sprintf("%.0f", 100 * .data$numerator / .data$denominator))
    ) |>
    tidyr::pivot_wider(id_cols = "answer", names_from = c("class", "gender"), values_from = "prop") |>
    dplyr::rename(`All\n%` = .data$All_All, ` ` = .data$answer) |>
    dplyr::rename_with(~ str_replace(.x, "(\\d)(?=_)", "\\1\n%")) |>
    flextable::flextable() |>
    flextable::separate_header() |>
    flextable::theme_vanilla() |>
    flextable::set_table_properties(layout = "autofit", width = 1) |>
    flextable::keep_with_next() |>
    flextable::set_caption(align_with_table = FALSE) |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::align(j = -1, align = "center", part = "all")
  if (any(summary_data$censored)) {
    tab |>
      flextable::add_footer_lines("* Numbers too low to show")
  } else {
    tab
  }
}

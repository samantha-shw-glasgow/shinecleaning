#' Collapsed summary (percentages of successes) for proportion graphs
#'
#' @param data Valid input data
#' @param var Variable to calculate by
#' @param success Character vector of categories as 'successes'
#' @param inc_gender List of genders to split by
#' @param inc_classes List of classes to split by
#' @param .censor Whether to censor (must be TRUE for production reports)
#' @param .gender_split Gender split - passed from params
#'
#' @return A dataframe of proportions/counts of successes
create_collapsed_summary <- function(
    data,
    var,
    success,
    inc_gender,
    inc_classes,
    .censor = FALSE,
    .gender_split = FALSE
) {
  if (.gender_split) {
    grouping_vars <- c("class", "gender")
  } else {
    grouping_vars <- c("class")
  }
  subgroups <- data |>
    group_by(across(all_of(grouping_vars))) |>
    mutate(success = {{var}} %in% success) |>
    summarise(
      numerator = sum(success, na.rm = TRUE),
      denom = n(),
      .groups = "drop"
    ) |>
    arrange(class)

  all <- subgroups |>
    summarise(
      class = "All",
      gender = "All",
      numerator = sum(numerator),
      denom = sum(denom)
    )

  if (.gender_split) {
    joined_dat <- subgroups |>
      filter(gender %in% inc_gender, class %in% inc_classes) |>
      bind_rows(all)
  } else {
    joined_dat <- all |>
      mutate(gender = "All pupils")
  }

  joined_dat |>
    transmute(
      class = forcats::fct_inorder(class),
      gender = replace_na(gender, "All"),
      numerator,
      denom
    )
}

#' Full summary counts across all categories
#'
#' @param data Valid input data
#' @param var Variable to calculate by
#' @param levels Character vector of ordered levels
#' @param inc_gender List of genders to split by
#' @param inc_classes List of classes to split by
#' @param .censor Whether to censor (must be TRUE for production reports)
#' @param .gender_split Gender split - passed from params
#'
#' @return A dataframe of counted variables
create_full_summary <- function(
    data,
    var,
    levels,
    inc_gender,
    inc_classes,
    .censor = FALSE,
    .gender_split = FALSE
) {
  var <- enquo(var)
  if (.gender_split) {
    grouping_vars <- c("class", "gender")
  } else {
    grouping_vars <- c("class")
  }
  subgroups <- data |>
    rename(answer = !!var) |>
    group_by(across(all_of(c(grouping_vars, "answer")))) |>
    summarise(numerator = n(), .groups = "drop") |>
    add_count(across(all_of(grouping_vars)), name = "denom", wt = numerator) |>
    filter(answer %in% levels) |>
    arrange(class)

  all <- subgroups |>
    summarise(
      class = "All",
      gender = "All",
      numerator = sum(numerator),
      .by = answer
    ) |>
    mutate(denom = sum(numerator))

  if (.gender_split) {
    joined_dat <- subgroups |>
      filter(gender %in% inc_gender, class %in% inc_classes) |>
      bind_rows(all)
  } else {
    joined_dat <- all
  }

   joined_dat |>
    transmute(
      class = forcats::fct_inorder(class),
      gender = replace_na(gender, "All"),
      answer = factor(answer, levels = levels),
      numerator,
      denom
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
    class = character(),
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
        class = factor(class, levels = levels(summary_data$class))
      ) |>
      select(class, gender, hbsc_gender, hbsc_prop)
  }

  summary_data |>
    mutate(prop = numerator / denom) |>
    left_join(hbsc_data_in, by = join_by(class, gender)) |>
    ggplot() +
    aes(
      x = class,
      y = prop,
      fill = gender,
      shape = hbsc_gender
    ) +
    geom_col(position = "dodge", size = 0) +
    {
      if (!is.null(hbsc_data))
        geom_point(
          aes(
            y = hbsc_prop,
            fill = hbsc_gender,
            colour = hbsc_gender
          ),
          position = position_dodge(0.9),
          size = 2
        )
    } +
    scale_shape_manual(
      values = c("Boys (Scotland)" =  21, "Girls (Scotland)" = 24),
      na.translate = FALSE,
      guide = guide_legend(
        order = 2,
        override.aes = list(
          fill = list("#fb1e20", "#008000"),
          colour = list(NA, NA)
        )
      )
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
      aes(label = scales::percent(
        .data$prop, suffix = "%", accuracy = 1
      )),
      color = "black",
      position = position_dodge(0.9),
      vjust = -0.5,
      size = 4
    ) +
    theme(
      plot.margin = unit(c(0.8, 0.5, 0.5, 1), "cm"),
      legend.title = element_blank(),
      legend.key = element_blank(),
      legend.box.background = element_blank()
    ) +
    coord_cartesian(ylim = c(0, 1), clip = "off")
}

#' A table of percentages from summary data
#'
#' @param summary_data A dataframe produced by `create_collapsed_summary`
#'
#' @return A printed `flextable`
#'
table_from_summary <- function(summary_data) {

  summary_data |>
    mutate(prop = sprintf("%.0f", 100*numerator/denom)) |>
    pivot_wider(id_cols = answer, names_from = c(class, gender), values_from = prop) |>
    rename(All = All_All, ` ` = answer) |>
    rename_with(~str_replace(.x, "(\\d)", "\\1\n%")) |>
    flextable() |>
    separate_header()|>
    theme_vanilla() |>
    set_table_properties(layout = "autofit", width = 1) |>
    set_caption(align_with_table = FALSE) |>
    align(j = -1, align = "center", part = "all")
}



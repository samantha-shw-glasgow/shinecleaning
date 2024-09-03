#' Collapsed summary (percentages of successes) for proportion graphs
#'
#' @param data Valid input data
#' @param var Variable to calculate by
#' @param success Character vector of categories as 'successes'
#' @param .censor Whether to censor (must be TRUE for production reports)
#' @param .gender_split Gender split - passed from params
#'
#' @return A dataframe of proportions/counts of successes
create_collapsed_summary <- function(
    data,
    var,
    success,
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

  bind_rows(subgroups, all) |>
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
#' @param .censor Whether to censor (must be TRUE for production reports)
#' @param .gender_split Gender split - passed from params
#'
#' @return A dataframe of counted variables
create_full_summary <- function(
    data,
    var,
    levels,
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
    arrange(class)

  all <- subgroups |>
    summarise(
      class = "All",
      gender = "All",
      numerator = sum(numerator),
      .by = answer
    ) |>
    mutate(denom = sum(numerator))

  bind_rows(subgroups, all) |>
    transmute(
      class = forcats::fct_inorder(class),
      gender = replace_na(gender, "All"),
      answer = factor(answer, levels = levels),
      numerator,
      denom
    )
}


#' Bar percentage from summary data
#'
#' @param summary_data A dataframe produced by `create_collapsed_summary`
#' @param inc_gender List of genders to include in table
#' @param inc_classes List of classes to include in table
#'
#' @return A ggplot2 graph
bar_from_summary <- function(summary_data, inc_gender = genders, hbsc_data = NULL) {
  summary_data |>
    filter(gender %in% inc_gender, class %in% inc_classes) |>
    mutate(prop = numerator/denom) |>
    ggplot() +
    aes(x = class, y = prop, fill = gender, colour = gender, shape = gender) +
    geom_col(position = "dodge", size = 0) +
    {if(!is.null(hbsc_data)) geom_point(data = hbsc_data,
                                        position = position_dodge(0.9),
                                        size = 2) } +
    scale_shape_manual(values = c("Boys (Scotland)" =  21, "Girls (Scotland)" = 24, "Boys" = NA, "Girls" = NA)) +
    scale_fill_hbsc() +
    scale_colour_hbsc() +
    xlab("") +
    scale_y_continuous("%", labels = scales::percent)+
    geom_text(aes(label = scales::percent(.data$prop, suffix="%", accuracy = 1)),
              color = "black",
              position = position_dodge(0.9),
              vjust = -0.5,
              size = 4) +
    theme(plot.margin = unit(c(0.8, 0.5, 0.5, 1),  "cm"),
          legend.title = element_blank()) +
    coord_cartesian(ylim = c(0, 1), clip = "off")
}

#' A table of percentages from summary data
#'
#' @param summary_data A dataframe produced by `create_collapsed_summary`
#' @param inc_gender List of genders to include in table
#'
#' @return A printed `flextable`
#'
table_from_summary <- function(summary_data, inc_gender) {

  summary_data |>
    filter(gender %in% inc_gender) |>
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



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

#' Table of class by gender counts
#'
#' @param data Valid school input data (with columns `gender` and `class`)
#' @param inc_gender List of genders to include in table
#' @param inc_classes List of classes to include in table
#'
#' @return A flextable giving gender by class counts
tab_categories <- function(data, inc_gender, inc_classes) {


  another_way <- sum(data$gender == "In another way")
  pnts <- sum(data$gender == "Prefer not to say")
  no_class <- sum(is.na(data$class) | data$class == "Prefer not to say")

  if (another_way) {
    cat(
      glue::glue(
        "{another_way} pupil{ifelse(another_way > 1,'s','')}",
        " identified 'In another way'. "
      )
    )
  }
  if (pnts) {
    cat(
      glue::glue(
        "{pnts} pupil{ifelse(pnts > 1,'s','')}",
        " did not provide details of their gender. "
      )
    )
  }
  if (no_class) {
    cat(
      glue::glue(
        "{pnts} pupil{ifelse(pnts > 1,'s','')}",
        " did not provide their year group. "
      )
    )
  }

  cat("\n")

  data |>
    filter(gender %in% inc_gender, class %in% inc_classes) |>
    count(gender, `Year group` = class) |>
    pivot_wider(names_from = gender, values_from = n) |>
    flextable() |>
    theme_vanilla() |>
    set_table_properties(layout = "autofit", width = 1) |>
    set_caption(align_with_table = FALSE)

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
  global_difficulties_colour <- "#a5a5a5"

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
      "Difficulties" = global_difficulties_colour,
      "Borderline" = global_elevated_colour,
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

#' Generate text to describe input date range
#'
#' @param input_data The dataframe of survey responses with the column `Start Date`
#'
#' @return A string describing month of survey (or range)
#'
date_range <- function(input_data) {

  start <- min(lubridate::ymd_hms(input_data$`Start Date`))
  end <- max(lubridate::ymd_hms(input_data$`Start Date`))

  start_date <- paste(lubridate::month(start, abbr = FALSE, label = TRUE), lubridate::year(start))
  end_date <- paste(lubridate::month(end, abbr = FALSE, label = TRUE), lubridate::year(end))

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
tab_responses <- function(input_data, n_invited) {

  n_responses <- nrow(input_data)
  n_missing <- n_invited - n_responses
  perc_rate <- 100 * n_responses/n_invited

  sprintf(
  "
  | | |
  |-|-|
  | Number of pupils invited to participate | %d     |
   | Number of pupils who did not take part  | %d     |
   | Overall response rate                   | %.0f%% |",
          n_invited, n_missing, perc_rate)

  tibble::tribble(
    ~a, ~b,
    "Number of pupils invited to participate", n_invited |> as.character(),
    "Number of pupils who did not take part", n_missing |> as.character(),
    "Overall response rate", sprintf("%.0f%%", perc_rate)
  )

}


bar_by_cat <- function(.data,
                       var,
                       success = "Yes",
                       .censor = TRUE,
                       .gender_split = TRUE) {

  require(tidyverse)

  # browser()

  var <- enquo(var)

  df_gender <- .data |>
    group_by(gender) |>
    mutate(success = {{var}} %in% success) |>
    summarise(numerator = sum(success, na.rm = TRUE),
              denom = n()) |>
    filter(!is.na(gender))

  if (((all(df_gender$numerator > 3) & all(df_gender$denom >= 7)) | !.censor) & .gender_split) {
    # * chart should not be created if there are ≤3 students in the numerator of
    #   any variable.
    # * only separate by gender if there are ≥7 girls AND ≥7 boys in the denominator
    #   of any variable

    p1 <- df_gender |>
      mutate(prop = numerator / denom) |>
      ggplot(aes(gender, prop, fill = gender)) +
      geom_bar(stat = "identity") +
      scale_fill_hbsc("") +
      xlab("") +
      scale_y_continuous("%", labels = scales::percent)+
      geom_text(aes(label = scales::percent(prop, suffix="%", accuracy = 1)),
                vjust = 0,
                nudge_y = 0.05,
                size = 4) +
      theme(plot.margin = unit(c(0.8, 0.5, 0.5, 1),  "cm")) +
      coord_cartesian(ylim = c(0, 1), clip = "off")

  } else { # Test semi-censored version

    # } else if (all(df_gender$numerator > 3) & sum(df_gender$denom <= 14)) {
    # * if there are ≤14 students, the chart should only present a single column
    #   representing all students.

    p1 <- .data |>
      mutate(success = !!var %in% success) |>
      summarise(prop = sum(success, na.rm = TRUE)/ n()) |>
      mutate(gender = "All pupils") |>
      ggplot(aes(gender, prop, fill = gender)) +
      geom_bar(stat = "identity") +
      scale_fill_hbsc("") +
      xlab("") +
      scale_y_continuous("%", labels = scales::percent)+
      geom_text(aes(label = scales::percent(prop, suffix="%", accuracy = 1)),
                vjust = 0,
                nudge_y = 0.05,
                size = 4) +
      theme(plot.margin = unit(c(0.8, 0.5, 0.5, 1),  "cm")) +
      coord_cartesian(ylim = c(0, 1), clip = "off")
  }
  # For full censoring
  # } else {
  #   p1 <- ggplot() +
  #     geom_text(aes(x = 1, y = 0.5, label = "Chart ommitted\ndue to low numbers"),
  #               size = 12) +
  #     scale_x_discrete(breaks = 1, labels = "") +
  #     scale_y_continuous("%", labels = percent, limits = c(0, 1))
  #
  # }

  df_school <- .data |>
    group_by(class) |>
    mutate(success = !!var %in% success) |>
    summarise(numerator = sum(success),
              denom = n(),
              .groups = "keep") |>
    filter(!is.na(class))

  if (((length(df_school$class) == 2 & all(df_gender$numerator > 3) & all(df_gender$denom >= 7) &
        all(df_school$denom >= 7)) | (length(df_school$class) == 2 & .censor == FALSE))) {
    # * for secondary schools, only separate by year if there are ≥7 S2 AND ≥7 S4).

    p2 <- df_school |>
      summarise(prop = sum(numerator) / sum(denom)) |>
      ggplot(aes(class, prop, fill = class)) +
      geom_bar(stat = "identity") +
      scale_fill_hbsc("") +
      xlab("") +
      scale_y_continuous("",
                         labels = NULL,
                         position = "right"
      ) +
      theme(axis.ticks.y = element_line(colour = "white"),
            axis.text.y = element_text(colour = "white"),
            plot.margin = unit(c(0.8, 0.5, 0.5, 1),  "cm")) +
      geom_text(aes(label = scales::percent(prop, suffix="%", accuracy = 1)),
                vjust = 0,
                nudge_y = 0.05,
                size = 4) +
      coord_cartesian(ylim = c(0, 1), clip = "off")
  } else {
    p2 <- NULL
  }

  p1 + p2
}

scale_fill_hbsc <- function(...) {

  primary_colour <-  "#4770b7"
  secondary_colour <- "#016bb2"
  main_colour <- "#333333"
  global_all_pupils_colour <- "#37474f"
  global_girls_colour <- "#88cbec"
  global_boys_colour <- "#4d648d"
  global_s2_colour <- "#548235"
  global_s4_colour <- "#C5E0B4"
  global_expected_colour <- "#4770b7"
  global_elevated_colour <- "#ee9457"

  scale_fill_manual(
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

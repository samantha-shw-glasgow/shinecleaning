#' Generate text to describe input date range
#'
#' @param input_data The dataframe of survey responses with the column `completed_date`
#'
#' @return A string describing month of survey (or range)
#'
date_range <- function(input_data) {
  start <- min(lubridate::ymd(input_data$completed_date))
  end <- max(lubridate::ymd(input_data$completed_date))

  start_date <- paste(
    lubridate::month(start, abbr = FALSE, label = TRUE),
    lubridate::year(start)
  )
  end_date <- paste(
    lubridate::month(end, abbr = FALSE, label = TRUE),
    lubridate::year(end)
  )

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
    flextable::flextable() |>
    flextable::delete_part("header") |>
    flextable::theme_vanilla() |>
    # border_inner_h(officer::fp_border("gray", width = 1)) |>
    flextable::set_table_properties(layout = "autofit", width = 1) |>
    flextable::set_caption("Response rate", align_with_table = FALSE)
}

#' Table of class by gender counts
#'
#' @param data Valid school input data (with columns `gender` and `class`)
#' @param inc_gender List of genders to include in table
#' @param inc_classes List of classes to include in table
#'
#'
#' @return A flextable giving gender by class counts
tab_categories <- function(data, inc_gender, inc_classes) {
  another_way <- sum(data$gender == "In another way", na.rm = TRUE)
  pnts <- sum(is.na(data$gender) | data$gender == "Prefer not to say")
  no_class <- sum(is.na(data$class) | data$class == "Prefer not to say")
  no_gender <- another_way + pnts

  if (no_gender) {
    cat(
      glue::glue(
        "{no_gender} pupil{ifelse(no_gender > 1,'s','')}",
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

  inc_classes <- purrr::list_c(as.list(inc_classes))

  data |>
    dplyr::filter(.data$gender %in% inc_gender, .data$class %in% inc_classes) |>
    dplyr::count(.data$gender, `Year group` = .data$class) |>
    tidyr::pivot_wider(names_from = "gender", values_from = "n") |>
    flextable::flextable() |>
    flextable::theme_vanilla() |>
    flextable::set_table_properties(layout = "autofit", width = 1) |>
    flextable::set_caption(align_with_table = FALSE)
}

#' Create a vector of nicely named, class groupings
#'
#' @param classes character vector of classes
#' @param groupings list of groups of classes, e.g. list(c("S1", "S2", "S3"), c("S4", "S5", "S6"))
#'
#' @return A character vector of class groupings

group_classes <- function(classes, groupings) {

  names(groupings) <- purrr::map(groupings, knitr::combine_words)

  purrr::map_chr(classes, function(old_class) {

      if (is.na(old_class) | old_class == "Prefer not to say") return(NA)

      class_group <- purrr::keep(groupings, ~ old_class %in% .x) |> names()

      if (length(class_group) == 0) {
        return("No group")
      }
      if (length(class_group) > 1) {
        rlang::warn("Multiple groups found for class ", old_class)
        return("Multiple groups")
      }

      return(class_group)
    })

}

#' Table of grouped classes by gender counts
#'
#' @param data Valid school input data (with columns `gender` and `class`)
#' @param inc_gender List of genders to include in table
#' @param class_groupings List of groups of classes to include in table
#'
#'
#' @return A flextable giving gender by grouped class counts
tab_categories_grouped <- function(data, inc_gender, class_groupings) {
  another_way <- sum(data$gender == "In another way", na.rm = TRUE)
  pnts <- sum(is.na(data$gender) | data$gender == "Prefer not to say")
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
    dplyr::filter(.data$gender %in% inc_gender, .data$class %in% unlist(class_groupings)) |>
    dplyr::mutate(
      classes_grouped = group_classes(class, class_groupings)
      ) |>
    dplyr::count(.data$gender, `Year group` = .data$classes_grouped) |>
    tidyr::pivot_wider(names_from = "gender", values_from = "n") |>
    flextable::flextable() |>
    flextable::theme_vanilla() |>
    flextable::set_table_properties(layout = "autofit", width = 1) |>
    flextable::set_caption(align_with_table = FALSE)
}

#' reshape hbsc summary data
#'
#' @param dat HBSC data to pass in
#' @param create_cols copy existing p7, s2 and s4 columns to fill missing years
#'
#' @return long-format dataframe
prep_hbsc <- function(dat, create_cols = FALSE) {
  if (create_cols) {
    dat <- dat |>
      dplyr::mutate(
        p6_boys = .data$p7_boys,
        p6_girls = .data$p7_girls,
        s1_boys = .data$s2_boys,
        s1_girls = .data$s2_girls,
        s3_boys = .data$s4_boys,
        s3_girls = .data$s4_girls,
        s5_boys = .data$s4_boys,
        s5_girls = .data$s4_girls,
        s6_boys = .data$s4_boys,
        s6_girls = .data$s4_girls,
      )
  }

  dat |>
    dplyr::mutate(
      fields2 = dplyr::case_match(
        .data$fields,
        "sch1_1" ~ "sch1_I like it a lot",
        "sch1_2" ~ "sch1_I like it a bit",
        "sch1_3" ~ "sch1_I don't like it very much",
        "sch1_4" ~ "sch1_I don't like it at all",
        "sch2_1" ~ "sch2_Not at all",
        "sch2_2" ~ "sch2_A little",
        "sch2_3" ~ "sch2_Some",
        "sch2_4" ~ "sch2_A lot",
        "sch3_1" ~ "sch3_Never",
        "sch3_2" ~ "sch3_Hardly ever",
        "sch3_3" ~ "sch3_Sometimes",
        "sch3_4" ~ "sch3_Often",
        "sch3_5" ~ "sch3_Always",
        .default = .data$fields
      ),
      q = stringr::str_split_i(.data$fields2, "_", 1),
      level = stringr::str_split_i(.data$fields2, "_", 2),
    ) |>
    dplyr::select(-"fields2") |>
    tidyr::pivot_longer(
      cols = -c("fields", "q", "level"),
      values_to = "prop",
      names_to = c("class", "gender"),
      names_sep = "_"
    ) |>
    dplyr::mutate(
      class = stringr::str_to_upper(.data$class),
      gender = dplyr::case_match(
        .data$gender,
        "boys" ~ "Boy",
        "girls" ~ "Girl",
        .default = .data$gender
      ),
      level = dplyr::case_match(
        .data$level,
        "excellent" ~ "Excellent",
        "good" ~ "Good",
        "fair" ~ "Fair",
        "poor" ~ "Poor",
        .default = .data$level
      ),
      q = dplyr::case_match(
        .data$q,
        "aswscore" ~ "asw_score",
        .default = .data$q
      ),
    )
}

#' Get the HBSC Scotland proportion by gender for provided response
#'
#' @param classes classes to include
#' @param success responses to include, can provide multiple
#' @param var (Optional) variable to include
#'
#' @return A tibble of proportion by gender
get_hbsc_prop <- function(classes, success, var = NULL) {
  if (is.null(var)) {
    var <- unique(SHINEcleaning::hbsc_scotland_modified$q[SHINEcleaning::hbsc_scotland_modified$level %in% success])
    if (length(var) > 1) {
      stop("Multiple variables found with response ", success, ". Please specify `var`.")
    }
  }

  classes <- unlist(classes)

  if (length(success) > 1) {
    data <- SHINEcleaning::hbsc_scotland_modified |>
      dplyr::filter(
        .data$level %in% success,
        .data$q %in% var,
        .data$class %in% classes
      ) |>
      dplyr::group_by(.data$class, .data$gender) |>
      dplyr::mutate(prop = sum(.data$prop))
  } else {
    data <- SHINEcleaning::hbsc_scotland_modified |>
      dplyr::filter(
        .data$level == success,
        .data$q %in% var,
        .data$class %in% classes
      )
  }

  out <- data |>
    dplyr::mutate(
      prop = .data$prop / 100,
      gender = dplyr::case_match(
        .data$gender,
        "Boy" ~ "Boys (Scotland)",
        "Girl" ~ "Girls (Scotland)"
      ),
    )

  return(out)
}

#' Get the HBSC Scotland score by gender for provided response
#'
#' @param classes classes to include
#' @param var variable to include
#'
#' @return A tibble of scores by gender
get_hbsc_score <- function(classes, var) {

  classes <- unlist(classes)

  data <- SHINEcleaning::hbsc_scotland_modified |>
    dplyr::filter(
      .data$q == var,
      .data$class %in% classes
    )

  out <- data |>
    dplyr::mutate(
      gender = dplyr::case_match(
        .data$gender,
        "Boy" ~ "Boys (Scotland)",
        "Girl" ~ "Girls (Scotland)"
      ),
    )

  return(out)
}

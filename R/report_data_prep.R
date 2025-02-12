#' Data preparation phase
#'
#' These functions prepare the data by reshaping it and generating necessary variables.
#' Helper functions for variables are included.
#'
#' @param survey_data The data to process
#' @param report_type The type of survey data ("primary" or "secondary")
#'
#' @importFrom rlang .data
#'
#' @returns
#' `data_prep`: A dataframe with the required variables for rendering a report
#'
#' The score-calculating functions return the dataset with relevant columns appended:
#'
#' `who_score`:  WHO 5-item wellbeing score (`who_score` variable) and categorical breakdown (`who_cat`: low/good)
#'
#' `mm_score`: 'Me and My feelings' score for primary schools
#'
#' `sehs_primary`: SEHS score for primary schools
#'
#' `sehs_secondary`: SEHS score for secondary schools
#'
#' `asw_score`: Adolescent sleep-wake score for secondary schools
#'
#' `sdq_score`: SDQ score for secondary schools
#'
#' `fas_score`: family affluence score (0-13)
#'
data_prep <- function(survey_data, report_type = "primary") {
  # This should create:
  #  - WHO5 wellbeing score - `who_score` and `who_cat`

  #  Primary:
  #  - 'Me and my feelings' scores - emotional and behavioural `mm_score`
  #  - 'Gratitude', 'Zest', 'Optimism', 'Persistence', 'Pro-social' - `sehs_primary`
  #  - Overall coviality `cov_score`
  #  - Family affluence score
  # Secondary:
  #  - secondary sehs (averaging by categories)
  #  - strenths and difficulties score - `sdq_score`
  #  - Adolescent sleep wake score - `asw_score`
  #  - Family affluence score
  #
  # It should also filter refusal to complete survey

  if (!("completed_date" %in% colnames(survey_data)) &&
      ("RecordedDate" %in% colnames(survey_data))) {
    survey_data$completed_date <- as.character(
      lubridate::parse_date_time(survey_data$RecordedDate,
                                 c("%Y-%m-%d %H:%M:%S", "%d/%m/%Y %H:%M")) |>
        as.Date())
  } else if (!("completed_date" %in% colnames(survey_data)) &&
             !("RecordedDate" %in% colnames(survey_data))) {
    stop("No date column found - please include `completed_date` or `RecordedDate`")
  }

  survey_out <- survey_data |>
    dplyr::filter(.data$consent == "Yes, I am happy to take part") |>
    dplyr::mutate(
      gender = .data$gender |>
      stringr::str_replace("(?<=(Boy|Girl))$", "s")) |> # pluralise for reporting
    dplyr::relocate("completed_date", .before = "consent") |>
    who_score()

  if (report_type == "primary") {
    if (!("mm1" %in% colnames(survey_out))) {
      stop(
        "Dataset is missing expected variables for primary report. ",
        "Did you correctly specify report type and are columns correctly named?"
      )
    }

    survey_out |>
      mm_score() |>
      sehs_primary() |>
      fas_score() |>
      dplyr::mutate(class = factor(.data$class, levels = c("P6", "P7")))
  } else if (report_type == "secondary") {
    if (!("asw1" %in% colnames(survey_out))) {
      stop(
        "Dataset is missing expected variables for secondary report. ",
        "Did you correctly specify report type and are columns correctly named?"
      )
    }

    survey_out |>
      sehs_secondary() |>
      asw_score() |>
      sdq_score() |>
      fas_score() |>
      dplyr::mutate(class = factor(.data$class, levels = c("S1", "S2", "S3", "S4", "S5", "S6")))
  } else {
    stop(glue::glue(
      "\"{report_type}\" is not a valid report type. ",
      "Specify \"primary\" or \"secondary\" to match data."
    ))
  }
}

#' @rdname data_prep
who_score <- function(survey_data) {
  # Sum the score of the five `Who` variables and multiply by 4

  who_responses <- c(
    "At no time",
    "Some of the time",
    "Less than half of the time",
    "More than half of the time",
    "Most of the time",
    "All of the time"
  )

  who_responses

  survey_data |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("who"), ~ match(.x, who_responses) - 1)) |>
    dplyr::mutate(
      who_score = rowSums(dplyr::pick(dplyr::starts_with("who"))) * 4,
      who_cat = dplyr::case_when(
        who_score <= 50 ~ "low",
        who_score > 50 ~ "good"
      ),
      who_dep = who_score <= 28,
      .keep = "none"
    ) |>
    dplyr::bind_cols(survey_data, x = _)
}

#' @rdname data_prep
mm_score <- function(survey_data) {
  mm_responses <- c(
    "Never",
    "Sometimes",
    "Always"
  )

  mm_responses

  # Add pro-rata correction if <1/3 missing

  # mme - ten columns, up to 3 missing
  # mmb - six columns, up to 2 missing

  survey_data |>
    dplyr::transmute(dplyr::across(dplyr::starts_with("mm"), ~ match(.x, mm_responses) - 1),
      mm15 = 2L - .data$mm15,
      mme_missing = rowSums(dplyr::pick(dplyr::matches(paste0("^mm", 1:10, "$"))) |> is.na()),
      mmb_missing = rowSums(dplyr::pick(dplyr::matches(paste0("^mm", 11:16, "$"))) |> is.na())
    ) |>
    dplyr::mutate(
      mme_score = rowSums(dplyr::pick(dplyr::matches(paste0("^mm", 1:10, "$"))), na.rm = TRUE),
      mmb_score = rowSums(dplyr::pick(dplyr::matches(paste0("^mm", 11:16, "$"))), na.rm = TRUE),
      mme_score = dplyr::case_when(
        mme_missing == 0 ~ mme_score,
        mme_missing <= 3 ~ 10 * mme_score / (10 - mme_missing),
        mme_missing > 3 ~ NA_real_
      ),
      mmb_score = dplyr::case_when(
        mmb_missing == 0 ~ mmb_score,
        mmb_missing <= 2 ~ 6 * mmb_score / (6 - mmb_missing),
        mmb_missing > 2 ~ NA_real_
      ),
      mme_cat = dplyr::if_else(.data$mme_score <= 9, "As expected", "Elevated"),
      mmb_cat = dplyr::if_else(.data$mmb_score <= 5, "As expected", "Elevated"),
      .keep = "none"
    ) |>
    dplyr::bind_cols(survey_data, x = _)
}

#' @rdname data_prep
sehs_primary <- function(survey_data) {
  # Calculates:
  # - "g","z","o","p","pro" scores as 4-wide column means
  # - summed to "coviality" score
  # Each must have 3 or more valid responses (pro-ratad)

  sehs_responses <- c(
    "Almost never",
    "Sometimes",
    "Often",
    "Very often"
  )

  sehs_responses

  num_vars <- survey_data |>
    dplyr::transmute(dplyr::across(dplyr::starts_with("sehs"), ~ match(.x, sehs_responses)))

  scores <- purrr::imap(c("g", "z", "o", "p", "pro"), \(variable, index) {
    start <- (index - 1) * 4 + 1

    num_vars |>
      dplyr::select(dplyr::matches(paste0("^sehs", start:(start + 3), "$"))) |>
      dplyr::mutate(
        score = rowSums(dplyr::pick(dplyr::matches(
          paste0("^sehs", start:(start + 3), "$")
        )), na.rm = TRUE),
        n_valid = rowSums(!is.na(dplyr::pick(dplyr::matches(
          paste0("^sehs", start:(start + 3), "$")
        ))))
      ) |>
      dplyr::mutate(
        "{variable}_score" := dplyr::case_when(
          n_valid == 4 ~ score,
          n_valid == 3 ~ 4 * score / 3,
          n_valid < 3 ~ NA_real_
        ),
        .keep = "none"
      )
  }) |>
    purrr::reduce(dplyr::bind_cols) |>
    dplyr::mutate(cov_score = .data$p_score + .data$g_score + .data$o_score + .data$z_score)

  dplyr::bind_cols(survey_data, scores)
}

#' @rdname data_prep
sehs_secondary <- function(survey_data) {
  sehs_responses <- c(
    "Not at all true of me",
    "A little true of me",
    "Pretty much true of me",
    "Very much true of me"
  )

  sehs_responses

  num_responses <- survey_data |>
    dplyr::transmute(dplyr::across(dplyr::starts_with("sehs"), ~ match(.x, sehs_responses)))


  scores <- purrr::imap(
    c(
      "efficacy",
      "aware",
      "persist",
      "sch_support",
      "fam_support",
      "peer_support",
      "emt_regulation",
      "empathy",
      "control",
      "optimism"
    ),
    \(variable, index) {
      start <- (index - 1) * 3 + 1

      rows_select <- paste0("^SEHSS", start:(start + 2), "$")

      num_responses |>
        dplyr::select(dplyr::matches(rows_select)) |>
        dplyr::mutate(
          score = rowSums(dplyr::pick(dplyr::matches(rows_select)), na.rm = TRUE),
          n_valid = rowSums(!is.na(dplyr::pick(
            dplyr::matches(rows_select)
          )))
        ) |>
        dplyr::mutate(
          "{variable}_score" := dplyr::case_when(
            n_valid == 3 ~ score,
            n_valid < 3 ~ NA_real_
          ),
          .keep = "none"
        )
    }
  ) |>
    purrr::reduce(dplyr::bind_cols) |>
    dplyr::mutate(
      belief_self_score = (.data$efficacy_score + .data$aware_score + .data$persist_score) / 3,
      belief_others_score = (.data$sch_support_score + .data$fam_support_score + .data$peer_support_score) / 3,
      emotional_competence_score = (.data$emt_regulation_score + .data$empathy_score + .data$control_score) / 3
    )

  dplyr::bind_cols(survey_data, scores)
}

#' @rdname data_prep
asw_score <- function(survey_data) {
  asw_responses <- c(
    "Never",
    "Once in a while",
    "Sometimes",
    "Quite often",
    "Frequently, if not always",
    "Always"
  )

  asw_responses

  survey_data |>
    dplyr::transmute(
      dplyr::across(dplyr::starts_with("ASW"), ~ match(.x, asw_responses)),
      # reverse scores for select vars
      dplyr::across(dplyr::matches(paste0("^ASW[1345678]$")), ~ 7 - .x)
    ) |>
    dplyr::mutate(asw_score = rowSums(dplyr::pick(dplyr::starts_with("ASW"))), .keep = "none") |>
    dplyr::bind_cols(survey_data, x = _)
}

#' @rdname data_prep
sdq_score <- function(survey_data) {

  sdq_cutoff <- list(
    ep = list(0:5, 6:6, 7:10),
    cp = list(0:3, 4:4, 5:10),
    ha = list(0:5, 6:6, 7:10),
    pp = list(0:3, 4:5, 6:10),
    ps = list(6:10, 5:5, 0:4),
    tot = list(0:15, 16:19, 20:40)
  )

  sdq_cutoff

  sdq_responses <- c(
    "Not true",
    "Somewhat true",
    "Certainly true"
  )

  sdq_responses

  num_responses <- survey_data |>
    dplyr::transmute(
      dplyr::across(dplyr::starts_with("SDQ"), ~ match(.x, sdq_responses) - 1),
      # reverse scores for select vars
      dplyr::across(dplyr::matches(paste0("^SDQ", c(7, 21, 25, 11, 14), "$")), ~ 2 - .x)
    )

  varlist <- list(
    ep = rlang::quo(dplyr::pick(dplyr::matches(paste0("^SDQ", c(3, 8, 13, 16, 24), "$")))),
    cp = rlang::quo(dplyr::pick(dplyr::matches(paste0("^SDQ", c(5, 7, 12, 18, 22), "$")))),
    ha = rlang::quo(dplyr::pick(dplyr::matches(paste0("^SDQ", c(2, 10, 15, 21, 25), "$")))),
    pp = rlang::quo(dplyr::pick(dplyr::matches(paste0("^SDQ", c(6, 11, 14, 19, 23), "$")))),
    ps = rlang::quo(dplyr::pick(dplyr::matches(paste0("^SDQ", c(1, 4, 9, 17, 20), "$"))))
  )

  varlist |>
    purrr::imap(\(score_vars, name) {
      num_responses |>
        dplyr::mutate(
          score = rowSums(!!score_vars, na.rm = TRUE),
          n_valid = rowSums(!is.na(!!score_vars))
        ) |>
        dplyr::mutate(
          corr_score = dplyr::if_else(.data$n_valid >= 3, .data$score / .data$n_valid * 5, NA_real_) |> round(),
          score_cat = dplyr::case_when(
            corr_score %in% sdq_cutoff[[name]][[1]] ~ "As expected",
            corr_score %in% sdq_cutoff[[name]][[2]] ~ "Borderline",
            corr_score %in% sdq_cutoff[[name]][[3]] ~ "Difficulties"
          )
        ) |>
        dplyr::transmute(
          "{name}_score" := .data$corr_score,
          "{name}_cat" := .data$score_cat
        )
    }) |>
    purrr::reduce(dplyr::bind_cols) |>
    dplyr::mutate(
      sdq_total_score = .data$ep_score + .data$cp_score + .data$ha_score + .data$pp_score,
      sdq_total_cat = dplyr::case_when(
        sdq_total_score %in% sdq_cutoff[["tot"]][[1]] ~ "As expected",
        sdq_total_score %in% sdq_cutoff[["tot"]][[2]] ~ "Borderline",
        sdq_total_score %in% sdq_cutoff[["tot"]][[3]] ~ "Difficulties"
      )
    ) |>
    dplyr::bind_cols(survey_data, x = _)
}

#' @rdname data_prep
fas_score <- function(survey_data) {

  survey_data |>
    dplyr::mutate(
      fas1 = factor(.data$fas1, levels = c("No", "Yes, one", "Yes, two or more")),
      fas2 = factor(.data$fas2, levels = c("No", "Yes")),
      fas3 = factor(.data$fas3, levels = c("None", "One", "Two", "More than two")),
      fas4 = factor(.data$fas4, levels = c(
        "Not at all", "Once", "Twice", "More than twice"
      )),
      fas5 = factor(.data$fas5, levels = c("None", "One", "Two", "More than two")),
      fas6 = factor(.data$fas6, levels = c("No", "Yes")),
      dplyr::across(dplyr::starts_with("fas"), as.integer),
      fas_score = as.integer(.data$fas1 + .data$fas2 + .data$fas3 + .data$fas4 + .data$fas5 + .data$fas6 - 6)
    ) |>
    dplyr::select(fas_score) |>
    dplyr::bind_cols(survey_data, x = _)

}

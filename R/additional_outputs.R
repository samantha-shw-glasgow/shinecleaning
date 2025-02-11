# create spreadsheet of data used to generate the report
report_data_spreadsheet <- function(data, filename, report_type) {
  #process data
  proc_data <- data |> data_prep(report_type)

  # added_columns <- c("completed_date")

  columns_to_remove <- c(
    "ResponseId",
    "StartDate",
    "EndDate",
    "RecordedDate",
    "Status",
    "Progress",
    "Duration (in seconds)",
    "Finished",
    "DistributionChannel",
    "UserLanguage",
    "Email",
    "postcode",
    "postcode_5_TEXT",
    "dobmnth",
    "dobday",
    "dobyr",
    "date_of_birth",
    "School contact",
    "School name"
  )

  #make spreadsheet
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet 1")
  proc_data |>
    dplyr::select(-dplyr::any_of(columns_to_remove)) |>
    dplyr::relocate(dplyr::any_of(c("Local Authority", "School ID code")), .before = "age") |>
  openxlsx::writeData(wb, 1, x = _)
  openxlsx::saveWorkbook(wb, filename)
}

# create spreadsheet of summary of variables grouped by school, class and gender
report_derived_spreadsheet <- function(data, filename, report_type, classes, genders = NULL) {

  if (is.null(genders)) {
    genders <- c("Boys", "Girls")
  }

  #process data
  proc_data <- data |> data_prep(report_type) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character),
                                ~dplyr::na_if(., "Prefer not to say")))

  #group by school and class
  grouped_data <- purrr::map(classes, \(concat_class) {
    proc_data |>
      dplyr::filter(.data$class %in% concat_class, .data$gender %in% genders) |>
      dplyr::mutate(
        class = stringr::str_flatten(concat_class, collapse = ", ", last = " and "),
        `Year groups` = stringr::str_c(.data$class, .data$gender, sep = " ")
      )
  }) |>
    purrr::reduce(dplyr::bind_rows) |>
    dplyr::group_by(.data$`School ID code`, .data$`Year groups`)

  #calculate summaries

  derived_data_all <- grouped_data |>
    dplyr::summarise(
      #all
      "Number taking part" = dplyr::n(),
      "% reporting good or excellent health" =
        mean(.data$health == "Good" | .data$health == "Excellent", na.rm = TRUE) * 100,
      "% reporting fair or poor health" =
        mean(.data$health == "Fair" | .data$health == "Poor", na.rm = TRUE) * 100,
      "Overall" =
        mean(valid_numbers(.data$lifesat1), na.rm = TRUE),
      "Family" =
        mean(valid_numbers(.data$lifesat2), na.rm = TRUE),
      "Home" =
        mean(valid_numbers(.data$lifesat3), na.rm = TRUE),
      "Choice" =
        mean(valid_numbers(.data$lifesat4), na.rm = TRUE),
      "Friends" =
        mean(valid_numbers(.data$lifesat5), na.rm = TRUE),
      "Things you have" =
        mean(valid_numbers(.data$lifesat6), na.rm = TRUE),
      "Health" =
        mean(valid_numbers(.data$lifesat7), na.rm = TRUE),
      "Appearance" =
        mean(valid_numbers(.data$lifesat8), na.rm = TRUE),
      "Future" =
        mean(valid_numbers(.data$lifesat9), na.rm = TRUE),
      "School" =
        mean(valid_numbers(.data$lifesat10), na.rm = TRUE),
      "Time use" =
        mean(valid_numbers(.data$lifesat11), na.rm = TRUE),
      "% low: Overall" =
        mean(valid_numbers(.data$lifesat1) < 5, na.rm = TRUE) * 100,
      "% low: Family" =
        mean(valid_numbers(.data$lifesat2) < 5, na.rm = TRUE) * 100,
      "% low: Home" =
        mean(valid_numbers(.data$lifesat3) < 5, na.rm = TRUE) * 100,
      "% low: Choice" =
        mean(valid_numbers(.data$lifesat4) < 5, na.rm = TRUE) * 100,
      "% low: Friends" =
        mean(valid_numbers(.data$lifesat5) < 5, na.rm = TRUE) * 100,
      "% low: Things you have" =
        mean(valid_numbers(.data$lifesat6) < 5, na.rm = TRUE) * 100,
      "% low: Health" =
        mean(valid_numbers(.data$lifesat7) < 5, na.rm = TRUE) * 100,
      "% low: Appearance" =
        mean(valid_numbers(.data$lifesat8) < 5, na.rm = TRUE) * 100,
      "% low: Future" =
        mean(valid_numbers(.data$lifesat9) < 5, na.rm = TRUE) * 100,
      "% low: School" =
        mean(valid_numbers(.data$lifesat10) < 5, na.rm = TRUE) * 100,
      "% low: Time use" =
        mean(valid_numbers(.data$lifesat11) < 5, na.rm = TRUE) * 100,
      "% reporting low mood" =
        mean(.data$who_cat == "low", na.rm = TRUE) * 100,
      "% reporting good mood" =
        mean(.data$who_cat == "good", na.rm = TRUE) * 100)

  if (report_type == "primary") {
    derived_data_additional <- grouped_data |>
      dplyr::summarise(
        "% scoring as expected-emotional" =
          mean(.data$mme_cat == "As expected", na.rm = TRUE) * 100,
        "% scoring elevated-emotional" =
          mean(.data$mme_cat == "Elevated", na.rm = TRUE) * 100,
        "% scoring as expected-behavioural" =
          mean(.data$mmb_cat == "As expected", na.rm = TRUE) * 100,
        "% scoring elevated-behavioural" =
          mean(.data$mmb_cat == "Elevated", na.rm = TRUE) * 100,
        "% who like school a lot or a bit" =
          mean(.data$sch1 == "I like it a lot" | .data$sch1 == "I like it a bit", na.rm = TRUE) * 100,
        "% who like school not very much or not at all" =
          mean(.data$sch1 == "I don\U2019t like it very much" | .data$sch1 == "I don\U2019t like it at all", na.rm = TRUE) * 100,
        "% who feel a lot or some pressure from schoolwork" =
          mean(.data$sch2 == "A lot" | .data$sch2 == "Some", na.rm = TRUE) * 100,
        "% who feel a little or no pressure from schoolwork" =
          mean(.data$sch2 == "A little" | .data$sch2 == "Not at all", na.rm = TRUE) * 100,
        "% who feel always or often confident" =
          mean(.data$sch3 == "Always" | .data$sch3 == "Often", na.rm = TRUE) * 100,
        "% who feel sometimes confident" =
          mean(.data$sch3 == "Sometimes", na.rm = TRUE) * 100,
        "% who feel never or hardly ever confident" =
          mean(.data$sch3 == "Never" | .data$sch3 == "Hardly ever", na.rm = TRUE) * 100,
        "Gratitude" =
          mean(valid_numbers(.data$g_score), na.rm = TRUE),
        "Zest" =
          mean(valid_numbers(.data$z_score), na.rm = TRUE),
        "Optimism" =
          mean(valid_numbers(.data$o_score), na.rm = TRUE),
        "Persistance" =
          mean(valid_numbers(.data$p_score), na.rm = TRUE),
        "Pro-social" =
          mean(valid_numbers(.data$pro_score), na.rm = TRUE),
        "Overall covitality score" =
          mean(valid_numbers(.data$cov_score), na.rm = TRUE)
        )
  } else if (report_type == "secondary") {
    derived_data_additional <- grouped_data |>
      dplyr::summarise(
        "% at risk of depression" =
          mean(.data$who_dep, na.rm = TRUE) * 100,
        "Emotional: % as expected" =
          mean(.data$ep_cat == "As expected", na.rm = TRUE) * 100,
        "Emotional: % borderline and difficulties" =
          mean(.data$ep_cat == "Borderline" | .data$ep_cat == "Difficulties", na.rm = TRUE) * 100,
        "Conduct: % as expected" =
          mean(.data$cp_cat == "As expected", na.rm = TRUE) * 100,
        "Conduct: % borderline and difficulties" =
          mean(.data$cp_cat == "Borderline" | .data$cp_cat == "Difficulties", na.rm = TRUE) * 100,
        "Hyperactivity: % as expected" =
          mean(.data$ha_cat == "As expected", na.rm = TRUE) * 100,
        "Hyperactivity: % borderline and difficulties" =
          mean(.data$ha_cat == "Borderline" | .data$ha_cat == "Difficulties", na.rm = TRUE) * 100,
        "Peer: % as expected" =
          mean(.data$pp_cat == "As expected", na.rm = TRUE) * 100,
        "Peer: % borderline and difficulties" =
          mean(.data$pp_cat == "Borderline" | .data$pp_cat == "Difficulties", na.rm = TRUE) * 100,
        "Pro-social: % as expected" =
          mean(.data$ps_cat == "As expected", na.rm = TRUE) * 100,
        "Pro-social: % borderline and difficulties" =
          mean(.data$ps_cat == "Borderline" | .data$ps_cat == "Difficulties", na.rm = TRUE) * 100,
        "Overall SDQ: % as expected" =
          mean(.data$sdq_total_cat == "As expected", na.rm = TRUE) * 100,
        "Overall SDQ: % borderline and difficulties" =
          mean(.data$sdq_total_cat == "Borderline" | .data$sdq_total_cat == "Difficulties", na.rm = TRUE) * 100,
        "Average sleep quality score" =
          mean(valid_numbers(.data$asw_score), na.rm = TRUE),
        "% who like school a lot or a bit" =
          mean(.data$sch1 == "I like it a lot" | .data$sch1 == "I like it a bit", na.rm = TRUE) * 100,
        "% who like school not very much or not at all" =
          mean(.data$sch1 == "I don\U2019t like it very much" | .data$sch1 == "I don\U2019t like it at all", na.rm = TRUE) * 100,
        "% who feel a lot or some pressure from schoolwork" =
          mean(.data$sch2 == "A lot" | .data$sch2 == "Some", na.rm = TRUE) * 100,
        "% who feel a little or no pressure from schoolwork" =
          mean(.data$sch2 == "A little" | .data$sch2 == "Not at all", na.rm = TRUE) * 100,
        "% who feel always or often confident" =
          mean(.data$sch3 == "Always" | .data$sch3 == "Often", na.rm = TRUE) * 100,
        "% who feel sometimes confident" =
          mean(.data$sch3 == "Sometimes", na.rm = TRUE) * 100,
        "% who feel never or hardly ever confident" =
          mean(.data$sch3 == "Never" | .data$sch3 == "Hardly ever", na.rm = TRUE) * 100,
        "Number asked selfh1" = sum(!is.na(.data$selfh1)),
        "% who have ever hurt themselves on purpose" =
          mean(.data$selfh1 == "Yes", na.rm = TRUE) * 100,
        "Number asked selfh2" = sum(!is.na(.data$selfh2)),
        "Of those who have hurt themselves, % who have not in the past year" =
          mean(.data$selfh2 == "None", na.rm = TRUE) * 100,
        "% who feel lonely none or some of the time" =
          mean(.data$loneliness == "None of the time" | .data$loneliness == "Some of the time", na.rm = TRUE) * 100,
        "% who feel lonely most or all of the time" =
          mean(.data$loneliness == "Most of the time" | .data$loneliness == "All of the time", na.rm = TRUE) * 100,
        "Self-efficacy" =
          mean(valid_numbers(.data$efficacy_score), na.rm = TRUE),
        "Self-awareness" =
          mean(valid_numbers(.data$aware_score), na.rm = TRUE),
        "Persistence" =
          mean(valid_numbers(.data$persist_score), na.rm = TRUE),
        "School support" =
          mean(valid_numbers(.data$sch_support_score), na.rm = TRUE),
        "Family support" =
          mean(valid_numbers(.data$fam_support_score), na.rm = TRUE),
        "Peer support" =
          mean(valid_numbers(.data$peer_support_score), na.rm = TRUE),
        "Emotional regulation" =
          mean(valid_numbers(.data$emt_regulation_score), na.rm = TRUE),
        "Empathy" =
          mean(valid_numbers(.data$empathy_score), na.rm = TRUE),
        "Self-control" =
          mean(valid_numbers(.data$control_score), na.rm = TRUE),
        "Optimism" =
          mean(valid_numbers(.data$optimism_score), na.rm = TRUE),
        "Belief in self" =
          mean(valid_numbers(.data$belief_self_score), na.rm = TRUE),
        "Belief in others" =
          mean(valid_numbers(.data$belief_others_score), na.rm = TRUE),
        "Emotional competence" =
          mean(valid_numbers(.data$emotional_competence_score), na.rm = TRUE),
      )
  }

  derived_data <- dplyr::full_join(derived_data_all, derived_data_additional, by = c("School ID code", "Year groups"))
  # Set all NaN values to NA so they display correctly in the spreadsheet
  for (col in names(derived_data)) {
    derived_data[[col]][is.nan(derived_data[[col]])] <- NA
  }

  # create header row
  if (report_type == "primary") {
    col_headers <- list(
      list("", "", ""),
      list("General health", ""),
      list("Happiness with life - average scores", "", "", "", "", "", "", "", "", "", ""),
      list("Happiness with life - % with a low score", "", "", "", "", "", "", "", "", "", ""),
      list("WHO Wellbeing Index", ""),
      list("Me and My Feelings", "", "", ""),
      list("Liking school", ""),
      list("Pressure from schoolwork", ""),
      list("Self-confidence", "", ""),
      list("Social Emotional Health - average scores", "", "", "", "", "")
    )
  } else if (report_type == "secondary")  {
    col_headers <- list(
      list("", "", ""),
      list("General health", ""),
      list("Happiness with life - average scores", "", "", "", "", "", "", "", "", "", ""),
      list("Happiness with life - % with a low score", "", "", "", "", "", "", "", "", "", ""),
      list("WHO Wellbeing Index", "", ""),
      list("Strengths and Difficulties Score", "", "", "", "", "", "", "", "", "", "", ""),
      list("Sleep quality"),
      list("Liking school", ""),
      list("Pressure from schoolwork", ""),
      list("Self-confidence", "", ""),
      list("Self-harm", "", "", ""),
      list("Loneliness", ""),
      list("Social Emotional Health - average scores", "", "", "", "", "", "", "", "", "", "", "", "")
    )
  }


  #make spreadsheet
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet 1")
  header_style <- openxlsx::createStyle(
    halign = "center",
    textDecoration = "bold",
    wrapText = TRUE
  )
  openxlsx::writeData(wb, 1, purrr::list_flatten(col_headers), startRow = 1)
  openxlsx::writeData(wb, 1, derived_data, startRow = 2, headerStyle = header_style)
  last_row <- nrow(derived_data) + 2

  purrr::walk(seq_along(col_headers),
       \(i)  {
         start <- sum(unlist(purrr::map(col_headers[1:i - 1], length))) + 1
         end <- sum(unlist(purrr::map(col_headers[1:i], length)))
         openxlsx::mergeCells(wb, 1, rows = 1, cols = start:end)

         border_style <- openxlsx::createStyle(
           border = "left", borderStyle = "medium"
         )
         openxlsx::addStyle(wb, 1, rows = 1, cols = start:end, style = header_style)
         openxlsx::addStyle(
           wb, 1, rows = 1:last_row, cols = start, style = border_style, stack = TRUE
         )
       })
  openxlsx::setColWidths(wb, 1, 4:ncol(derived_data), 12)
  openxlsx::addStyle(wb, 1, rows = 3:last_row, cols = 4:ncol(derived_data),
                     gridExpand = TRUE, stack = TRUE,
                     style = openxlsx::createStyle(numFmt = "0.0"))
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
}

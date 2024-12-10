# create spreadsheet of data used to generate the report
report_data_spreadsheet <- function(data, filename, report_type) {
  #process data
  proc_data <- data |> data_prep(report_type)

  #make spreadsheet
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet 1")
  openxlsx::writeData(wb, 1, proc_data)
  openxlsx::saveWorkbook(wb, filename)
}

# create spreadsheet of summary of variables grouped by school, class and gender
report_derived_spreadsheet <- function(data, filename, report_type, classes, genders = NULL) {

  if (is.null(genders)) {
    genders <- c("Boys", "Girls")
  }

  #process data
  proc_data <- data |> data_prep(report_type) |>
    mutate(across(where(is.character), ~na_if(., "Prefer not to say")))

  #group by school and class
  grouped_data <- map(classes, \(concat_class) {
    proc_data |>
      filter(class %in% concat_class, gender %in% genders) |>
      mutate(
        class = str_flatten(concat_class, collapse = ", ", last = " and "),
        `Year groups` = str_c(class, gender, sep = " ")
      )
  }) |>
    reduce(bind_rows) |>
    group_by(`School ID code`, `Year groups`)

  #calculate summaries

  derived_data_all <- grouped_data |>
    summarise(
      #all
      "Number taking part" = n(),
      "% reporting good or excellent health" = mean(health == "Good" | health == "Excellent", na.rm = TRUE) * 100,
      "% reporting fair or poor health" = mean(health == "Fair" | health == "Poor", na.rm = TRUE) * 100,
      "Overall" = mean(valid_numbers(lifesat1), na.rm = TRUE),
      "Family" = mean(valid_numbers(lifesat2), na.rm = TRUE),
      "Home" = mean(valid_numbers(lifesat3), na.rm = TRUE),
      "Choice" = mean(valid_numbers(lifesat4), na.rm = TRUE),
      "Friends" = mean(valid_numbers(lifesat5), na.rm = TRUE),
      "Things you have" = mean(valid_numbers(lifesat6), na.rm = TRUE),
      "Health" = mean(valid_numbers(lifesat7), na.rm = TRUE),
      "Appearance" = mean(valid_numbers(lifesat8), na.rm = TRUE),
      "Future" = mean(valid_numbers(lifesat9), na.rm = TRUE),
      "School" = mean(valid_numbers(lifesat10), na.rm = TRUE),
      "Time use" = mean(valid_numbers(lifesat11), na.rm = TRUE),
      "% low: Overall" = mean(valid_numbers(lifesat1) < 5, na.rm = TRUE) * 100,
      "% low: Family" = mean(valid_numbers(lifesat2) < 5, na.rm = TRUE) * 100,
      "% low: Home" = mean(valid_numbers(lifesat3) < 5, na.rm = TRUE) * 100,
      "% low: Choice" = mean(valid_numbers(lifesat4) < 5, na.rm = TRUE) * 100,
      "% low: Friends" = mean(valid_numbers(lifesat5) < 5, na.rm = TRUE) * 100,
      "% low: Things you have" = mean(valid_numbers(lifesat6) < 5, na.rm = TRUE) * 100,
      "% low: Health" = mean(valid_numbers(lifesat7) < 5, na.rm = TRUE) * 100,
      "% low: Appearance" = mean(valid_numbers(lifesat8) < 5, na.rm = TRUE) * 100,
      "% low: Future" = mean(valid_numbers(lifesat9) < 5, na.rm = TRUE) * 100,
      "% low: School" = mean(valid_numbers(lifesat10) < 5, na.rm = TRUE) * 100,
      "% low: Time use" = mean(valid_numbers(lifesat11) < 5, na.rm = TRUE) * 100,
      "% reporting low mood" = mean(who_cat == "low", na.rm = TRUE) * 100,
      "% reporting good mood" = mean(who_cat == "good", na.rm = TRUE) * 100)

  if (report_type == "primary") {
    derived_data_additional <- grouped_data |>
      summarise(
        "% scoring as expected-emotional" = mean(mme_cat == "As expected", na.rm = TRUE) * 100,
        "% scoring elevated-emotional" = mean(mme_cat == "Elevated", na.rm = TRUE) * 100,
        "% scoring as expected-behavioural" = mean(mmb_cat == "As expected", na.rm = TRUE) * 100,
        "% scoring elevated-behavioural" = mean(mmb_cat == "Elevated", na.rm = TRUE) * 100,
        "% who like school a lot or a bit" = mean(sch1 == "I like it a lot" | sch1 == "I like it a bit", na.rm = TRUE) * 100,
        "% who like school not very much or not at all" = mean(sch1 == "I don’t like it very much" | sch1 == "I don’t like it at all", na.rm = TRUE) * 100,
        "% who feel a lot or some pressure from schoolwork" = mean(sch2 == "A lot" | sch2 == "Some", na.rm = TRUE) * 100,
        "% who feel a little or no pressure from schoolwork" = mean(sch2 == "A little" | sch2 == "Not at all", na.rm = TRUE) * 100,
        "% who feel always or often confident" = mean(sch3 == "Always" | sch3 == "Often", na.rm = TRUE) * 100,
        "% who feel sometimes confident" = mean(sch3 == "Sometimes", na.rm = TRUE) * 100,
        "% who feel never or hardly ever confident" = mean(sch3 == "Never" | sch3 == "Hardly ever", na.rm = TRUE) * 100,
        "Gratitude" = mean(valid_numbers(g_score), na.rm = TRUE),
        "Zest" = mean(valid_numbers(z_score), na.rm = TRUE),
        "Optimism" = mean(valid_numbers(o_score), na.rm = TRUE),
        "Persistance" = mean(valid_numbers(p_score), na.rm = TRUE),
        "Pro-social" = mean(valid_numbers(pro_score), na.rm = TRUE),
        "Overall covitality score" = mean(valid_numbers(cov_score), na.rm = TRUE)
        )
  } else if (report_type == "secondary") {
    derived_data_additional <- grouped_data |>
      summarise(
        "% at risk of depression" = mean(who_dep, na.rm = TRUE) * 100,
        "Emotional: % as expected" = mean(ep_cat == "As expected", na.rm = TRUE) * 100,
        "Emotional: % borderline and difficulties" = mean(ep_cat == "Borderline" | ep_cat == "Difficulties", na.rm = TRUE) * 100,
        "Conduct: % as expected" = mean(cp_cat == "As expected", na.rm = TRUE) * 100,
        "Conduct: % borderline and difficulties" = mean(cp_cat == "Borderline" | cp_cat == "Difficulties", na.rm = TRUE) * 100,
        "Hyperactivity: % as expected" = mean(ha_cat == "As expected", na.rm = TRUE) * 100,
        "Hyperactivity: % borderline and difficulties" = mean(ha_cat == "Borderline" | ha_cat == "Difficulties", na.rm = TRUE) * 100,
        "Peer: % as expected" = mean(pp_cat == "As expected", na.rm = TRUE) * 100,
        "Peer: % borderline and difficulties" = mean(pp_cat == "Borderline" | pp_cat == "Difficulties", na.rm = TRUE) * 100,
        "Pro-social: % as expected" = mean(ps_cat == "As expected", na.rm = TRUE) * 100,
        "Pro-social: % borderline and difficulties" = mean(ps_cat == "Borderline" | ps_cat == "Difficulties", na.rm = TRUE) * 100,
        "Overall SDQ: % as expected" = mean(sdq_total_cat == "As expected", na.rm = TRUE) * 100,
        "Overall SDQ: % borderline and difficulties" = mean(sdq_total_cat == "Borderline" | sdq_total_cat == "Difficulties", na.rm = TRUE) * 100,
        "Average sleep quality score" = mean(valid_numbers(asw_score), na.rm = TRUE),
        "% who like school a lot or a bit" = mean(sch1 == "I like it a lot" | sch1 == "I like it a bit", na.rm = TRUE) * 100,
        "% who like school not very much or not at all" = mean(sch1 == "I don’t like it very much" | sch1 == "I don’t like it at all", na.rm = TRUE) * 100,
        "% who feel a lot or some pressure from schoolwork" = mean(sch2 == "A lot" | sch2 == "Some", na.rm = TRUE) * 100,
        "% who feel a little or no pressure from schoolwork" = mean(sch2 == "A little" | sch2 == "Not at all", na.rm = TRUE) * 100,
        "% who feel always or often confident" = mean(sch3 == "Always" | sch3 == "Often", na.rm = TRUE) * 100,
        "% who feel sometimes confident" = mean(sch3 == "Sometimes", na.rm = TRUE) * 100,
        "% who feel never or hardly ever confident" = mean(sch3 == "Never" | sch3 == "Hardly ever", na.rm = TRUE) * 100,
        "Number asked selfh1" = sum(!is.na(selfh1)),
        "% who have ever hurt themselves on purpose" = mean(selfh1 == "Yes", na.rm = TRUE) * 100,
        "Number asked selfh2" = sum(!is.na(selfh2)),
        "Of those who have hurt themselves, % who have not in the past year" = mean(selfh2 == "None", na.rm = TRUE) * 100,
        "% who feel lonely none or some of the time" = mean(loneliness == "None of the time" | loneliness == "Some of the time", na.rm = TRUE) * 100,
        "% who feel lonely most or all of the time" = mean(loneliness == "Most of the time" | loneliness == "All of the time", na.rm = TRUE) * 100,
        "Self-efficacy" = mean(valid_numbers(efficacy_score), na.rm = TRUE),
        "Self-awareness" = mean(valid_numbers(aware_score), na.rm = TRUE),
        "Persistence" = mean(valid_numbers(persist_score), na.rm = TRUE),
        "School support" = mean(valid_numbers(sch_support_score), na.rm = TRUE),
        "Family support" = mean(valid_numbers(fam_support_score), na.rm = TRUE),
        "Peer support" = mean(valid_numbers(peer_support_score), na.rm = TRUE),
        "Empathy" = mean(valid_numbers(empathy_score), na.rm = TRUE),
        "Self-control" = mean(valid_numbers(control_score), na.rm = TRUE),
        "Optimism" = mean(valid_numbers(optimism_score), na.rm = TRUE),
        "Belief in self" = mean(valid_numbers(belief_self_score), na.rm = TRUE),
        "Belief in others" = mean(valid_numbers(belief_others_score), na.rm = TRUE),
        "Emotional competence" = mean(valid_numbers(emotional_competence_score), na.rm = TRUE),
      )
  }

  derived_data <- full_join(derived_data_all, derived_data_additional, by = c("School ID code", "Year groups"))
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
      list("Strengths and Difficulties Score", "", "", "", "", "", "", "", "", "", "", "", ""),
      list("Sleep quality"),
      list("Liking school", ""),
      list("Pressure from schoolwork", ""),
      list("Self-confidence", "", ""),
      list("Self-harm", "", "", ""),
      list("Loneliness", ""),
      list("Social Emotional Health - average scores", "", "", "", "", "", "", "", "", "", "", "", "", "")
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
  openxlsx::writeData(wb, 1, list_flatten(col_headers), startRow = 1)
  openxlsx::writeData(wb, 1, derived_data, startRow = 2, headerStyle = header_style)
  last_row <- nrow(derived_data) + 2

  walk(seq_along(col_headers),
       \(i)  {
         start <- sum(unlist(map(col_headers[1:i - 1], length))) + 1
         end <- sum(unlist(map(col_headers[1:i], length)))
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

test_that("Excel generation works - primary", {
  pri_test_a <- readr::read_csv(test_path("raw_data", "pri_test_small.csv"),
                                show_col_types = FALSE)[-1:-2, ]

  out_file <- file.path(tempdir(), "primary_stats_a.xlsx")

  report_derived_spreadsheet(
    pri_test_a,
    report_type = "primary",
    classes = c("P6", "P7"),
    filename = out_file
  )
  expect_true(file.exists(out_file))

})

test_that("Excels return values matching report - primary", {

  pri_test_a <- readr::read_csv(test_path("raw_data", "pri_test_small.csv"),
                                show_col_types = FALSE)[-1:-2, ] |>
    data_prep("primary") |>
    dplyr::mutate(dplyr::across(dplyr::matches(c("health", "sch\\d")),
                                ~ dplyr::na_if(., "Prefer not to say")))


  all_columns <- pri_test_a |>
    dplyr::group_by(.data$`School ID code`) |>
    (\(input_data) {
      dplyr::left_join(
        .summarise_common_cols(input_data),
        .summarise_primary_cols(input_data),
        by = "School ID code"
      )
    })()

  extract_perc_vals <- function(data, var, summary_function, success) {
    tab_out <- summary_function(data, {{var}}, success, genders = "All", classes = "x")
    100 * tab_out$numerator / tab_out$denominator
  }

  expected <- tibble::tibble(
    `School ID code` =
      unique(pri_test_a$`School ID code`),
    `Number taking part` =
      nrow(pri_test_a),
    `% reporting good or excellent health` =
      extract_perc_vals(pri_test_a, health, create_collapsed_summary, c("Good", "Excellent")) |> round(0),
    `% reporting fair or poor health` =
      extract_perc_vals(pri_test_a, health, create_collapsed_summary, c("Fair", "Poor")) |> round(0),
    `Overall` =
      summary_mean_multiple_vars(pri_test_a, list(lifesat1 = "Overall"))[[1]]$mean,
    `Family` =
      summary_mean_multiple_vars(pri_test_a, list(lifesat2 = "Family"))[[1]]$mean,
    `Home` =
      summary_mean_multiple_vars(pri_test_a, list(lifesat3 = "Home"))[[1]]$mean,
    `Choice` =
      summary_mean_multiple_vars(pri_test_a, list(lifesat4 = "Choice"))[[1]]$mean,
    `Friends` =
      summary_mean_multiple_vars(pri_test_a, list(lifesat5 = "Friends"))[[1]]$mean,
    `Things you have` =
      summary_mean_multiple_vars(pri_test_a, list(lifesat6 = "Things you have"))[[1]]$mean,
    `Health` =
      summary_mean_multiple_vars(pri_test_a, list(lifesat7 = "Health"))[[1]]$mean,
    `Appearance` =
      summary_mean_multiple_vars(pri_test_a, list(lifesat8 = "Appearance"))[[1]]$mean,
    `Future` =
      summary_mean_multiple_vars(pri_test_a, list(lifesat9 = "Future"))[[1]]$mean,
    `School` =
      summary_mean_multiple_vars(pri_test_a, list(lifesat10 = "School"))[[1]]$mean,
    `Time use` =
      summary_mean_multiple_vars(pri_test_a, list(lifesat11 = "Time use"))[[1]]$mean,
    `% low: Overall` =
      100 * summary_proportions_multiple(pri_test_a, list(lifesat1 = "Overall"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: Family` =
      100 * summary_proportions_multiple(pri_test_a, list(lifesat2 = "Family"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: Home` =
      100 * summary_proportions_multiple(pri_test_a, list(lifesat3 = "Home"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: Choice` =
      100 * summary_proportions_multiple(pri_test_a, list(lifesat4 = "Choice"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: Friends` =
      100 * summary_proportions_multiple(pri_test_a, list(lifesat5 = "Friends"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: Things you have` =
      100 * summary_proportions_multiple(pri_test_a, list(lifesat6 = "Things you have"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: Health` =
      100 * summary_proportions_multiple(pri_test_a, list(lifesat7 = "Health"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: Appearance` =
      100 * summary_proportions_multiple(pri_test_a, list(lifesat8 = "Appearance"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: Future` =
      100 * summary_proportions_multiple(pri_test_a, list(lifesat9 = "Future"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: School` =
      100 * summary_proportions_multiple(pri_test_a, list(lifesat10 = "School"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: Time use` =
      100 * summary_proportions_multiple(pri_test_a, list(lifesat11 = "Time use"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% reporting low mood` =
      extract_perc_vals(pri_test_a, who_cat, create_collapsed_summary, "low"),
    `% reporting good mood` =
      extract_perc_vals(pri_test_a, who_cat, create_collapsed_summary, "good"),
    `% scoring as expected-emotional` =
      100 * share_elevated(pri_test_a, mme_cat)$prop[2],
    `% scoring elevated-emotional` =
      100 * share_elevated(pri_test_a, mme_cat)$prop[1],
    `% scoring as expected-behavioural` =
      100 * share_elevated(pri_test_a, mmb_cat)$prop[2],
    `% scoring elevated-behavioural` =
      100 * share_elevated(pri_test_a, mmb_cat)$prop[1],
    `% who like school a lot or a bit` =
      extract_perc_vals(pri_test_a, sch1, create_collapsed_summary, c("I like it a lot", "I like it a bit")),
    `% who like school not very much or not at all` =
      extract_perc_vals(pri_test_a, sch1, create_collapsed_summary, c("I don\U2019t like it very much", "I don\U2019t like it at all")),
    `% who feel a lot or some pressure from schoolwork` =
      extract_perc_vals(pri_test_a, sch2, create_collapsed_summary, c("A lot", "Some")),
    `% who feel a little or no pressure from schoolwork` =
      extract_perc_vals(pri_test_a, sch2, create_collapsed_summary, c("A little", "Not at all")),
    `% who feel always or often confident` =
      extract_perc_vals(pri_test_a, sch3, create_collapsed_summary, c("Always", "Often")),
    `% who feel sometimes confident` =
      extract_perc_vals(pri_test_a, sch3, create_collapsed_summary, c("Sometimes")),
    `% who feel never or hardly ever confident` =
      extract_perc_vals(pri_test_a, sch3, create_collapsed_summary, c("Never", "Hardly ever")),
    `Gratitude` =
      summary_mean_multiple_vars(pri_test_a, list(g_score = "Gratitude"))[[1]]$mean,
    `Zest` =
      summary_mean_multiple_vars(pri_test_a, list(z_score = "Zest"))[[1]]$mean,
    `Optimism` =
      summary_mean_multiple_vars(pri_test_a, list(o_score = "Optimism"))[[1]]$mean,
    `Persistance` =
      summary_mean_multiple_vars(pri_test_a, list(p_score = "Persistance"))[[1]]$mean,
    `Pro-social` =
      summary_mean_multiple_vars(pri_test_a, list(pro_score = "Pro-social"))[[1]]$mean,
    `Overall covitality score` =
      summary_mean_single_var(pri_test_a, cov_score)$mean_score
  )

  expect_equal(
    all_columns,
    expected
  )
})

test_that("Excel generation works - secondary", {
  sec_test_a <- readr::read_csv(test_path("raw_data", "sec_test_large.csv"),
                                show_col_types = FALSE)[-1:-2, ]

  out_file <- file.path(tempdir(), "secondary_stats_a.xlsx")

  report_derived_spreadsheet(
    sec_test_a,
    report_type = "secondary",
    classes = list(c("S1", "S2"), c("S3", "S4")),
    filename = out_file
  )
  expect_true(file.exists(out_file))
})


test_that("Excels return values matching report - secondary", {

  sec_test_a <- readr::read_csv(test_path("raw_data", "sec_test_large.csv"),
                                show_col_types = FALSE)[-1:-2, ] |>
    data_prep("secondary") |>
    dplyr::mutate(dplyr::across(dplyr::matches(c("health", "sch\\d", "loneliness")),
                                ~ dplyr::na_if(., "Prefer not to say")))


  all_columns <- sec_test_a |>
    dplyr::group_by(.data$`School ID code`) |>
    (\(input_data) {
      dplyr::left_join(
        .summarise_common_cols(input_data),
        .summarise_secondary_cols(input_data),
        by = "School ID code"
      )
    })()

  extract_perc_vals <- function(data, var, summary_function, success) {
    tab_out <- summary_function(data, {{var}}, success, genders = "All", classes = "x")
    100 * tab_out$numerator / tab_out$denominator
  }

  expected <- tibble::tibble(
    `School ID code` =
      unique(sec_test_a$`School ID code`),
    `Number taking part` =
      nrow(sec_test_a),
    `% reporting good or excellent health` =
      extract_perc_vals(sec_test_a, health, create_collapsed_summary, c("Good", "Excellent")),
    `% reporting fair or poor health` =
      extract_perc_vals(sec_test_a, health, create_collapsed_summary, c("Fair", "Poor")),
    `Overall` =
      summary_mean_multiple_vars(sec_test_a, list(lifesat1 = "Overall"))[[1]]$mean,
    `Family` =
      summary_mean_multiple_vars(sec_test_a, list(lifesat2 = "Family"))[[1]]$mean,
    `Home` =
      summary_mean_multiple_vars(sec_test_a, list(lifesat3 = "Home"))[[1]]$mean,
    `Choice` =
      summary_mean_multiple_vars(sec_test_a, list(lifesat4 = "Choice"))[[1]]$mean,
    `Friends` =
      summary_mean_multiple_vars(sec_test_a, list(lifesat5 = "Friends"))[[1]]$mean,
    `Things you have` =
      summary_mean_multiple_vars(sec_test_a, list(lifesat6 = "Things you have"))[[1]]$mean,
    `Health` =
      summary_mean_multiple_vars(sec_test_a, list(lifesat7 = "Health"))[[1]]$mean,
    `Appearance` =
      summary_mean_multiple_vars(sec_test_a, list(lifesat8 = "Appearance"))[[1]]$mean,
    `Future` =
      summary_mean_multiple_vars(sec_test_a, list(lifesat9 = "Future"))[[1]]$mean,
    `School` =
      summary_mean_multiple_vars(sec_test_a, list(lifesat10 = "School"))[[1]]$mean,
    `Time use` =
      summary_mean_multiple_vars(sec_test_a, list(lifesat11 = "Time use"))[[1]]$mean,
    `% low: Overall` =
      100 * summary_proportions_multiple(sec_test_a, list(lifesat1 = "Overall"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: Family` =
      100 * summary_proportions_multiple(sec_test_a, list(lifesat2 = "Family"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: Home` =
      100 * summary_proportions_multiple(sec_test_a, list(lifesat3 = "Home"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: Choice` =
      100 * summary_proportions_multiple(sec_test_a, list(lifesat4 = "Choice"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: Friends` =
      100 * summary_proportions_multiple(sec_test_a, list(lifesat5 = "Friends"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: Things you have` =
      100 * summary_proportions_multiple(sec_test_a, list(lifesat6 = "Things you have"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: Health` =
      100 * summary_proportions_multiple(sec_test_a, list(lifesat7 = "Health"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: Appearance` =
      100 * summary_proportions_multiple(sec_test_a, list(lifesat8 = "Appearance"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: Future` =
      100 * summary_proportions_multiple(sec_test_a, list(lifesat9 = "Future"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: School` =
      100 * summary_proportions_multiple(sec_test_a, list(lifesat10 = "School"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% low: Time use` =
      100 * summary_proportions_multiple(sec_test_a, list(lifesat11 = "Time use"), ~ valid_numbers(.x) < 5)[[1]]$prop,
    `% reporting low mood` =
      extract_perc_vals(sec_test_a, who_cat, create_collapsed_summary, "low"),
    `% reporting good mood` =
      extract_perc_vals(sec_test_a, who_cat, create_collapsed_summary, "good"),
    `% at risk of depression` =
      extract_perc_vals(sec_test_a, .data$who_dep, create_collapsed_summary, TRUE),
    `Emotional: % as expected` =
      100 * share_elevated(sec_test_a, .data$ep_cat, c("Difficulties", "Borderline", "As expected"))$prop[1],
    `Emotional: % borderline and difficulties` =
      100 * share_elevated(sec_test_a, .data$ep_cat, c("Difficulties", "Borderline", "As expected"))$prop[2:3] |> sum(),
    `Conduct: % as expected` =
      100 * share_elevated(sec_test_a, .data$cp_cat, c("Difficulties", "Borderline", "As expected"))$prop[1],
    `Conduct: % borderline and difficulties` =
      100 * share_elevated(sec_test_a, .data$cp_cat, c("Difficulties", "Borderline", "As expected"))$prop[2:3] |> sum(),
    `Hyperactivity: % as expected` =
      100 * share_elevated(sec_test_a, .data$ha_cat, c("Difficulties", "Borderline", "As expected"))$prop[1],
    `Hyperactivity: % borderline and difficulties` =
      100 * share_elevated(sec_test_a, .data$ha_cat, c("Difficulties", "Borderline", "As expected"))$prop[2:3] |> sum(),
    `Peer: % as expected` =
      100 * share_elevated(sec_test_a, .data$pp_cat, c("Difficulties", "Borderline", "As expected"))$prop[1],
    `Peer: % borderline and difficulties` =
      100 * share_elevated(sec_test_a, .data$pp_cat, c("Difficulties", "Borderline", "As expected"))$prop[2:3] |> sum(),
    `Pro-social: % as expected` =
      100 * share_elevated(sec_test_a, .data$ps_cat, c("Difficulties", "Borderline", "As expected"))$prop[1],
    `Pro-social: % borderline and difficulties` =
      100 * share_elevated(sec_test_a, .data$ps_cat, c("Difficulties", "Borderline", "As expected"))$prop[2:3] |> sum(),
    `Overall SDQ: % as expected` =
      100 * share_elevated(sec_test_a, .data$sdq_total_cat, c("Difficulties", "Borderline", "As expected"))$prop[1],
    `Overall SDQ: % borderline and difficulties` =
      100 * share_elevated(sec_test_a, .data$sdq_total_cat, c("Difficulties", "Borderline", "As expected"))$prop[2:3] |> sum(),
    `Average sleep quality score` =
      summary_mean_single_var(sec_test_a, .data$asw_score)$mean_score,
    `% who like school a lot or a bit` =
      extract_perc_vals(sec_test_a, sch1, create_collapsed_summary, c("I like it a lot", "I like it a bit")),
    `% who like school not very much or not at all` =
      extract_perc_vals(sec_test_a, sch1, create_collapsed_summary, c("I don\U2019t like it very much", "I don\U2019t like it at all")),
    `% who feel a lot or some pressure from schoolwork` =
      extract_perc_vals(sec_test_a, sch2, create_collapsed_summary, c("A lot", "Some")),
    `% who feel a little or no pressure from schoolwork` =
      extract_perc_vals(sec_test_a, sch2, create_collapsed_summary, c("A little", "Not at all")),
    `% who feel always or often confident` =
      extract_perc_vals(sec_test_a, sch3, create_collapsed_summary, c("Always", "Often")),
    `% who feel sometimes confident` =
      extract_perc_vals(sec_test_a, sch3, create_collapsed_summary, c("Sometimes")),
    `% who feel never or hardly ever confident` =
      extract_perc_vals(sec_test_a, sch3, create_collapsed_summary, c("Never", "Hardly ever")),
    `Number asked about self-harm` =
      sum(!is.na(dplyr::filter(sec_test_a, .data$class %in% c("S3", "S4", "S5", "S6"))$selfh1)),
    `% who have ever hurt themselves on purpose` =
      100 * share_elevated(sec_test_a, .data$selfh1, c("Yes", "Prefer not to say", "No"))$prop[3],
    `% who feel lonely none or some of the time` =
      extract_perc_vals(sec_test_a, .data$loneliness, create_collapsed_summary, c("None of the time", "Some of the time")),
    `% who feel lonely most or all of the time` =
      extract_perc_vals(sec_test_a, .data$loneliness, create_collapsed_summary, c("Most of the time", "All of the time")),
    `Self-efficacy` =
      summary_mean_single_var(sec_test_a, .data$efficacy_score)$mean_score,
    `Self-awareness` =
      summary_mean_single_var(sec_test_a, .data$aware_score)$mean_score,
    `Persistence` =
      summary_mean_single_var(sec_test_a, .data$persist_score)$mean_score,
    `School support` =
      summary_mean_single_var(sec_test_a, .data$sch_support_score)$mean_score,
    `Family support` =
      summary_mean_single_var(sec_test_a, .data$fam_support_score)$mean_score,
    `Peer support` =
      summary_mean_single_var(sec_test_a, .data$peer_support_score)$mean_score,
    `Emotional regulation` =
      summary_mean_single_var(sec_test_a, .data$emt_regulation_score)$mean_score,
    `Empathy` =
      summary_mean_single_var(sec_test_a, .data$empathy_score)$mean_score,
    `Self-control` =
      summary_mean_single_var(sec_test_a, .data$control_score)$mean_score,
    `Optimism` =
      summary_mean_single_var(sec_test_a, .data$optimism_score)$mean_score,
    `Belief in self` =
      summary_mean_single_var(sec_test_a, .data$belief_self_score)$mean_score,
    `Belief in others` =
      summary_mean_single_var(sec_test_a, .data$belief_others_score)$mean_score,
    `Emotional competence` =
      summary_mean_single_var(sec_test_a, .data$emotional_competence_score)$mean_score
  )

  expect_equal(
    all_columns,
    expected
  )
})

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
    data_prep("primary")


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
    round(100 * tab_out$numerator / tab_out$denominator, 0)
  }

  expected <- tibble::tibble(
    `School ID code` = unique(pri_test_a$`School ID code`),
    `Number taking part` = nrow(pri_test_a),
    `% reporting good or excellent health` = extract_perc_vals(pri_test_a, health, create_collapsed_summary, c("Good", "Excellent")),
    `% reporting fair or poor health` = extract_perc_vals(pri_test_a, health, create_collapsed_summary, c("Fair", "Poor")),
    `Overall` = summary_mean_multiple_vars(pri_test_a, list(lifesat1 = "Overall"))[[1]]$mean,
    `Family` = summary_mean_multiple_vars(pri_test_a, list(lifesat2 = "Family"))[[1]]$mean,
    `Home` = summary_mean_multiple_vars(pri_test_a, list(lifesat3 = "Home"))[[1]]$mean,
    `Choice` = summary_mean_multiple_vars(pri_test_a, list(lifesat4 = "Choice"))[[1]]$mean,
    `Friends` = summary_mean_multiple_vars(pri_test_a, list(lifesat5 = "Friends"))[[1]]$mean,
    `Things you have` = summary_mean_multiple_vars(pri_test_a, list(lifesat6 = "Things you have"))[[1]]$mean,
    `Health` = summary_mean_multiple_vars(pri_test_a, list(lifesat7 = "Health"))[[1]]$mean,
    `Appearance` = summary_mean_multiple_vars(pri_test_a, list(lifesat8 = "Appearance"))[[1]]$mean,
    `Future` = summary_mean_multiple_vars(pri_test_a, list(lifesat9 = "Future"))[[1]]$mean,
    `School` = summary_mean_multiple_vars(pri_test_a, list(lifesat10 = "School"))[[1]]$mean,
    `Time use` = summary_mean_multiple_vars(pri_test_a, list(lifesat11 = "Time use"))[[1]]$mean,
    # `% low: Overall` = ,
    # `% low: Family` = ,
    # `% low: Home` = ,
    # `% low: Choice` = ,
    # `% low: Friends` = ,
    # `% low: Things you have` = ,
    # `% low: Health` = ,
    # `% low: Appearance` = ,
    # `% low: Future` = ,
    # `% low: School` = ,
    # `% low: Time use` = ,
    # `% reporting low mood` = ,
    # `% reporting good mood` = ,
    # `% scoring as expected-emotional` = ,
    # `% scoring elevated-emotional` = ,
    # `% scoring as expected-behavioural` = ,
    # `% scoring elevated-behavioural` = ,
    # `% who like school a lot or a bit` = ,
    # `% who like school not very much or not at all` = ,
    # `% who feel a lot or some pressure from schoolwork` = ,
    # `% who feel a little or no pressure from schoolwork` = ,
    # `% who feel always or often confident` = ,
    # `% who feel sometimes confident` = ,
    # `% who feel never or hardly ever confident` = ,
    # `Gratitude` = ,
    # `Zest` = ,
    # `Optimism` = ,
    # `Persistance` = ,
    # `Pro-social` = ,
    # `Overall covitality score` =
  )

  expect_equal(
    all_columns[,1:15],
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

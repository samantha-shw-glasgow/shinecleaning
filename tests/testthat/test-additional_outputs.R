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

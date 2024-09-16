test_that("primary report - sample data", {
  pri_test_a <- readr::read_csv(test_path("raw_data", "pri_test_small.csv"),
    show_col_types = FALSE
  )[-1:-2, ]

  out_dir <- tempdir()

  render_report(pri_test_a, school_name = "Test School A", output_location = out_dir, filename = "primary_report_a.docx")

  expect_true(
    file.exists(file.path(out_dir, "primary_report_a.docx"))
  )
})


test_that("Expects school or LA name", {
  expect_error(render_report(school_name = "a", local_authority_name = "b"))
  expect_error(render_report())
})

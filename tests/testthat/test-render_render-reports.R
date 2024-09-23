test_that("primary report - sample data", {
  pri_test_a <- readr::read_csv(test_path("raw_data", "pri_test_small.csv"),
    show_col_types = FALSE
  )[-1:-2, ]

  out_dir <- tempdir()


  render_report(
    pri_test_a,
    survey_type = "primary",
    school_name = "Test School A",
    output_location = out_dir,
    filename = "primary_report_a.docx"
  )
  expect_true(
    file.exists(file.path(out_dir, "primary_report_a.docx"))
  )
})

test_that("secondary report - sample data", {
  sec_test_a <- readr::read_csv(test_path("raw_data", "sec_test_large.csv"),
                                show_col_types = FALSE)[-1:-2, ]

  out_dir <- tempdir()

  render_report(
    sec_test_a,
    survey_type = "secondary",
    school_name = "Test School A",
    output_location = out_dir,
    filename = "secondary_report_a.docx"
  )

  expect_true(file.exists(file.path(out_dir, "secondary_report_a.docx")))
})


test_that("Expects school or LA name", {
  expect_error(render_report(school_name = "a", local_authority_name = "b"))
  expect_error(render_report())
})

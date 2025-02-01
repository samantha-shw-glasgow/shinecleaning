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
    classes = c("P6", "P7"),
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
    classes = list(c("S1", "S2"), c("S3", "S4")),
    filename = "secondary_report_a.docx"
  )

  expect_true(file.exists(file.path(out_dir, "secondary_report_a.docx")))
})

test_that("primary report - missing class", {
  pri_test_a <- readr::read_csv(test_path("raw_data", "pri_test_small.csv"),
    show_col_types = FALSE
  )[-1:-2, ] |>
    dplyr::mutate(class = "P7")

  out_dir <- tempdir()


  render_report(
    pri_test_a,
    survey_type = "primary",
    school_name = "Test School A",
    output_location = out_dir,
    filename = "primary_report_b.docx"
  )
  expect_true(
    file.exists(file.path(out_dir, "primary_report_b.docx"))
  )
})

test_that("secondary report - missing upper classes", {
  sec_test_a <- readr::read_csv(test_path("raw_data", "sec_test_large.csv"),
                                show_col_types = FALSE)[-1:-2, ] |>
    dplyr::filter(class %in% c("S1", "S2", "S3"))

  out_dir <- tempdir()

  render_report(
    sec_test_a,
    survey_type = "secondary",
    school_name = "Test School A",
    output_location = out_dir,
    filename = "secondary_report_b.docx"
  )

  expect_true(file.exists(file.path(out_dir, "secondary_report_b.docx")))
})


test_that("Expects school or LA name", {
  expect_error(render_report(school_name = "a", local_authority_name = "b"))
  expect_error(render_report())
})

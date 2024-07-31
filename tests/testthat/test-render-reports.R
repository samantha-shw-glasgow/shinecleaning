test_that("primary report - sample data", {

  pri_test_a <- readr::read_csv(system.file("testdata", "primary-school-full.csv", package = "SHINEcleaning"), skip = 1)[-1,]
  pri_test_b <- readr::read_csv(system.file("testdata", "primary-school-small.csv", package = "SHINEcleaning"), skip = 1)[-1,]

  out_dir <- tempdir()

  render_report(pri_test_a, school_name = "Test School A", output_location = out_dir, filename = "primary_report_a.docx")
  render_report(pri_test_b, school_name = "Test School B", output_location = out_dir, filename = "primary_report_b.docx")

  expect_true(
    file.exists(file.path(out_dir, "primary_report_a.docx")) &
    file.exists(file.path(out_dir, "primary_report_b.docx"))
  )

})

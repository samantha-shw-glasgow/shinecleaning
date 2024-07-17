create_spreadsheet <- function(data) {
  data <- data.frame(
    id = 1:5,
    dob = c(
      lubridate::ymd("2010-01-01"),
      lubridate::ymd("2010-01-02"),
      lubridate::ymd("2010-01-03"),
      lubridate::ymd("2010-01-03"),
      lubridate::ymd("2010-01-04")
    )
  )

  output <- run_validations(data, validators = list(check_duplicate_dob))

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet 1")
  openxlsx::writeData(wb, 1, output, withFilter = TRUE)
  openxlsx::setColWidths(wb, 1, cols = ncol(output), widths = "auto")

  openxlsx::saveWorkbook(wb, file = "test.xlsx", overwrite = TRUE)
}

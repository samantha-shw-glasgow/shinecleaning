create_spreadsheet <- function(data, file) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet 1")
  openxlsx::writeData(wb, 1, data, withFilter = TRUE)
  openxlsx::setColWidths(wb, 1, cols = ncol(data), widths = "auto")
  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
  file
}

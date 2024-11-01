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

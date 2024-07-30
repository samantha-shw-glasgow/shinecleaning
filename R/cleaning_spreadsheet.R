create_spreadsheet <- function(data, file) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet 1")
  openxlsx::writeData(wb, 1, data, withFilter = TRUE)

  date_cols <- which(grepl("Date", names(data)))
  openxlsx::setColWidths(wb, 1, cols = date_cols, widths = 19)
  openxlsx::setColWidths(wb, 1, cols = 1:3, widths = c(30, 12, 30))

  openxlsx::freezePane(wb, 1, firstActiveRow = 2, firstActiveCol = 4)

  # Highlight error messages
  last_row <- nrow(data) + 1
  openxlsx::conditionalFormatting(
    wb,
    1,
    cols = 1,
    rows = 2:last_row,
    type = "notBlanks",
    style = openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  )
  # Only allow "0" or "1" in "keep?" column
  openxlsx::dataValidation(
    wb,
    1,
    col = 2,
    rows = 2:last_row,
    type = "whole",
    operator = "between",
    value = c(0, 1)
  )
  # Show excluded rows as greyed out
  openxlsx::conditionalFormatting(
    wb,
    1,
    cols = 1:ncol(data),
    rows = 1:last_row,
    rule = "$B1==0",
    style = openxlsx::createStyle(fontColour = "#AAAAAA", bgFill = "#EEEEEE")
  )

  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
  file
}

create_spreadsheet <- function(data, file) {

  pseud_id_columns <- c(
    "Local Authority",
    "School ID code"
  )

  added_columns <- c(
    "completed_date",
    "age"
  )
  columns_to_redact <- c(
    "StartDate",
    "EndDate",
    "RecordedDate",
    "Status",
    "Progress",
    "Duration (in seconds)",
    "Finished",
    "DistributionChannel",
    "UserLanguage",
    "Email",
    "postcode",
    "postcode_5_TEXT",
    "dobmnth",
    "dobday",
    "dobyr",
    "date_of_birth",
    "School contact",
    "School name"
  )
  # Move columns to redact and added columns to the left of the spreadsheet
  data <- data |>
    dplyr::relocate(dplyr::any_of(added_columns), .after = "Reviewer notes") |>
    dplyr::relocate(dplyr::any_of(pseud_id_columns), .after = "Reviewer notes") |>
    dplyr::relocate(dplyr::any_of(columns_to_redact), .after = "Reviewer notes")

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
    style = openxlsx::createStyle(fontColour = "#000000", bgFill = "#FFC7CE")
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
  # Highlight added columns
  openxlsx::addStyle(
    wb,
    1,
    cols = match(added_columns, names(data)),
    rows = 1:last_row,
    style = openxlsx::createStyle(fontColour = "#000000", fgFill = "#C6EFCE"),
    gridExpand = TRUE
  )
  # Highlight columns to be redacted
  openxlsx::addStyle(
    wb,
    1,
    cols = match(columns_to_redact, names(data)),
    rows = 1:last_row,
    style = openxlsx::createStyle(fontColour = "#DD0000", fgFill = "#EEEEEE"),
    gridExpand = TRUE
  )

  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
  file
}

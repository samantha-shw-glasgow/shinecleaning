#' Run all upload checks for raw data or for clean data
#'
#' @param data
#'
#' @return dataframe of failed checks
#'
#' @keywords internal
upload_checks_raw <- function(data, vars) {
  checks <- rbind(
    upcheck_status(data),
    upcheck_schools(data),
    upcheck_gender(data),
    upcheck_has_columns(data, vars, required = FALSE)
  )

  return(checks[checks$fail, c("message", "level")])
}

upload_checks_clean <- function(data, vars) {
  checks <- rbind(
    upcheck_has_columns(data, vars, required = TRUE),
    upcheck_unique_response_ids(data),
    upcheck_schools(data)
  )

  return(checks[checks$fail, c("message", "level")])
}


#' Data upload checks
#'
#' @param data
#'
#' @return single row data.frame of columns fail (logical), message (character), level (integer)
#'
#' @keywords internal
upcheck_status <- function(data) {
  fail <- FALSE
  message <- NA
  level <- NA

  if (!"Status" %in% colnames(data)) {
    fail <- TRUE
    message <- "`Status` (response type) column not found in this file. Unable to check for test responses."
    level <- 2
  } else {
    if ("Survey Preview" %in% data$Status) {
      fail <- TRUE
      message <- "This file includes preview / test responses."
      level <- 1
    }
  }

  return(data.frame(fail, message, level))
}

upcheck_schools <- function(data) {
  fail <- FALSE
  message <- NA
  level <- NA

  if (!"School ID code" %in% colnames(data)) {
    fail <- TRUE
    message <- "`School ID code` column not found in this file. Please include school ID in the uploaded file."
    level <- 3
  } else {
    schIDs <- data$`School ID code`
    if (length(unique(schIDs)) > 1) {
      fail <- TRUE
      message <- "The data includes responses from multiple schools."
      level <- 1
    }
  }

  return(data.frame(fail, message, level))
}

upcheck_gender <- function(data) {
  fail <- FALSE
  message <- NA
  level <- NA

  if (!"gender" %in% colnames(data)) {
    fail <- TRUE
    message <- "`gender` column not found in this file. Please include gender in the uploaded file."
    level <- 3
  } else {
    genders <- data$gender
    if (any(stringr::str_detect(genders, "[0-9]"), na.rm = TRUE)) {
      fail <- TRUE
      message <- "This file has numeric values for gender where characters are expected. Please ensure you have downloaded the data with full text responses."
      level <- 3
    }
  }

  return(data.frame(fail, message, level))
}

upcheck_has_columns <- function(data, columns, required = TRUE) {
  fail <- FALSE
  message <- NA
  level <- NA

  has_col <- columns %in% colnames(data)

  if (!all(has_col)) {
    fail <- TRUE
    if (isTRUE(required)) {
      message <- paste0("This file is missing the following required column(s): ", paste0(columns[!has_col], collapse = ", "))
      level <- 3
    } else {
      message <- paste0("This file is missing the following expected column(s): ", paste0(columns[!has_col], collapse = ", "))
      level <- 2
    }
  }

  return(data.frame(fail, message, level))
}

upcheck_unique_response_ids <- function(data) {
  fail <- FALSE
  message <- NA
  level <- NA

  if (!"ResponseId" %in% colnames(data)) {
    fail <- TRUE
    message <- "`ResponseId` column not found in this file. Unable to check for duplicate responses."
    level <- 2
  } else {
    if (length(unique(data$ResponseId)) < nrow(data)) {
      fail <- TRUE
      message <- "The uploaded file(s) includes duplicate response IDs."
      level <- 2
    }
  }

  return(data.frame(fail, message, level))
}

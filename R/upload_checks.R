#' Run all upload checks for raw data or for clean data
#'
#' @param data
#'
#' @return dataframe of failed checks
#'
#' @keywords internal
upload_checks_raw <- function(data){
  checks <- rbind(
    upcheck_status(data),
    upcheck_schools(data),
    upcheck_gender(data)
    )

  return(checks[checks$fail, c("message", "level")])
}

upload_checks_clean <- function(data, vars){
  checks <- rbind(
    #upcheck_has_columns(data, vars),
    upcheck_schools(data)
  )

  return(checks[checks$fail, c("message", "level")])
}

#' Make upload warning UI
#'
#' @param message character
#' @param level integer, 1-3
#'
#' @return div html object
#'
#' @keywords internal
make_upload_warning <- function(message, level){
  if(level==1){
    return(
      tags$div(span(icon("circle-exclamation"),
                  message),
             class="card p-2 m-2 text-info")
    )
  }

  if(level==2){
    return(
      tags$div(span(icon("circle-question"),
                  message),
             class="card p-2 m-2 text-warning")
    )
  }

  if(level==3){
    return(
      tags$div(span(icon("triangle-exclamation"),
                  message),
             class="card p-2 m-2 text-danger")
    )
  }
}

#' Data upload checks
#'
#' @param data
#'
#' @return single row data.frame of columns fail (logical), message (character), level (integer)
#'
#' @keywords internal
upcheck_status <- function(data){
  fail = FALSE
  message = NA
  level = NA

  if(! "Status" %in% colnames(data)){
    fail = TRUE
    message = "`Status` (response type) column not found in this file. Unable to check for test responses."
    level = 2
  } else {
    if("Survey Preview" %in% data$Status){
      fail = TRUE
      message = "This file includes preview / test responses."
      level = 1
    }
  }

  return(data.frame(fail, message, level))
}

upcheck_schools <- function(data){
  fail = FALSE
  message = NA
  level = NA

  if(! "School ID code" %in% colnames(data)){
    fail = TRUE
    message = "`School ID code` column not found in this file. Please include school ID in the uploaded file."
    level = 3
  } else {
    schIDs = data$`School ID code`
    if(length(unique(schIDs)) > 1){
      fail = TRUE
      message = "This file includes responses from multiple schools."
      level = 1
    }
  }

  return(data.frame(fail, message, level))
}

upcheck_gender <- function(data){
  fail = FALSE
  message = NA
  level = NA

  if(! "gender" %in% colnames(data)){
    fail = TRUE
    message = "`gender` column not found in this file. Please include gender in the uploaded file."
    level = 3
  } else {
    genders = data$gender
    if(any(stringr::str_detect(genders, "[0-9]"), na.rm = TRUE)){
      fail = TRUE
      message = "This file has numeric values for gender where characters are expected. Please ensure you have downloaded the data with full text responses."
      level = 3
    }
  }

  return(data.frame(fail, message, level))
}

upcheck_has_columns <- function(data, columns){
  fail = FALSE
  message = NA
  level = NA

  has_col = columns %in% colnames(data)

  if (! all(isTRUE(has_col))) {
    fail = TRUE
    message = paste0("This file is missing the following required column(s): ", paste0(columns[!has_col], collapse = ", "))
    level = 3
  }

  return(data.frame(fail, message, level))
}


#' Run all upload checks for raw data
#'
#' @param data
#'
#' @return dataframe of failed checks
#'
#' @keywords internal
upload_checks <- function(data){
  checks <- rbind(
    upcheck_status(data),
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
             class="card p-2")
    )
  }

  if(level==2){
    return(
      tags$div(span(icon("circle-question"),
                  message),
             class="card p-2 bg-warning")
    )
  }

  if(level==3){
    return(
      tags$div(span(icon("triangle-exclamation"),
                  message),
             class="card p-2 bg-danger")
    )
  }
}

#' Data upload checks
#'
#' @param data
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

  if(! "SchID" %in% colnames(data)){
    fail = TRUE
    message = "`SchID` (school ID) column not found in this file. Please include school ID in the uploaded file."
    level = 3
  } else {
    schIDs = data$SchID[!data$SchID %in% c("{\"ImportId\":\"SchID\"}", "SchID")]
    if(length(unique(schIDs)) > 1){
      fail = TRUE
      message = "This file includes responses from multiple schools."
      level = 1
    }
  }

  return(data.frame(fail, message, level))
}

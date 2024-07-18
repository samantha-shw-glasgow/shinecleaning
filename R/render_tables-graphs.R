date_range <- function(input_data) {

  start <- min(lubridate::ymd_hms(input_data$StartDate))
  end <- max(lubridate::ymd_hms(input_data$StartDate))

  start_date <- paste(lubridate::month(start, abbr = FALSE, label = TRUE), lubridate::year(start))
  end_date <- paste(lubridate::month(end, abbr = FALSE, label = TRUE), lubridate::year(end))

  if (start_date == end_date) {
    paste("in", start_date)
  } else {
    paste("from", start_date, "to", end_date)
  }

}


tab_responses <- function(input_data) {

  responses <- nrow(input_data)

}

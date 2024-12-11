#' Calculate age from date of birth and date of record
#'
#' @param record_date Date data collected (in format "%Y-%m-%d %H:%M:%S" or "%d/%m/%Y %H:%M")
#' @param dobyr Year of birth
#' @param dobmnth Month of birth
#' @param dobday Day of birth
#'
#' @return An integer vector of ages
calculate_age <- function(record_date, dobyr, dobmnth, dobday) {
    dob <- lubridate::ymd(paste(dobyr, dobmnth, dobday), quiet = TRUE)
    record_date <- lubridate::parse_date_time(record_date, c("%Y-%m-%d %H:%M:%S", "%d/%m/%Y %H:%M")) |> as.Date()
    age <- round(lubridate::decimal_date(record_date) - lubridate::decimal_date(dob), 2)

    return(age)
}

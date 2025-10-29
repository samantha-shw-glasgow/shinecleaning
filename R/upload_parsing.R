parse_raw_csv <- function(path) {
  readr::read_csv(path,
                        col_types = readr::cols(.default = "c"),
                        show_col_types = F
  )[-1:-2, ]
}



read_dob_column <- function(data) {
  # Surveys in 24-25 year have `dobyr`, `dobmnth` and `dobday`
  # Surveys from Oct '25 have `dob_1` in place of above
  # Surveys from Nov '25 have `dob_2#1_1`, `dob_2#2_1`, `dob_2#3_1` also
  # Only one of the latter two will be completed if present

  data_out <- data

  if (!all(c("dobyr", "dobmnth", "dobday") %in% colnames(data))) {
    # If data contains `dob_1`, this indicates survey is Oct '25 pilots
    if ("dob_1" %in% colnames(data)) {
      # If data contains `dob_2`, this indicates survey is beyond Oct '25 pilots
      if (all(is.na(data$dob_1)) &&
          "dob_2#1_1" %in% colnames(data)) {
        data_out <- data |>
          dplyr::mutate(
            dobyr = as.integer(.data$`dob_2#3_1`),
            dobmnth = match(.data$`dob_2#2_1`, month.name),
            dobday = as.integer(.data$`dob_2#1_1`)
          )

      } else {
        data_out <- data |>
          dplyr::mutate(
            dob_parse = as.character(lubridate::parse_date_time(.data$dob_1, c("%Y-%m-%d %H:%M:%S", "%d/%m/%Y %H:%M")) |> as.Date()),
            dobyr = lubridate::year(.data$dob_parse),
            dobmnth = lubridate::month(.data$dob_parse),
            dobday = lubridate::day(.data$dob_parse)
          ) |>
          dplyr::select(-dob_parse)
      }


    }
  }

  data_out

}

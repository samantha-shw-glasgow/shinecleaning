parse_raw_csv <- function(path) {
  readr::read_csv(path,
                        col_types = readr::cols(.default = "c"),
                        show_col_types = F
  )[-1:-2, ]
}


read_dob_column <- function(data) {

  data_out <- data

  if (!all(c("dobyr", "dobmnth", "dobday") %in% colnames(data))) {

    if ("dob_1" %in% colnames(data))

      data_out <- data |>
      dplyr::mutate(
        dobyr = lubridate::year(.data$dob_1),
        dobmnth = lubridate::month(.data$dob_1),
        dobday = lubridate::day(.data$dob_1)
      )

  }

  data_out

}

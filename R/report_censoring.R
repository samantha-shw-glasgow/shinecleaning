#' @import dplyr
#' @importFrom rlang .data
censor_summary_data <- function(summary_data, col_name) {
  summary_data |>
    dplyr::mutate(
      censored = .data[[col_name]] < 3
    )
}

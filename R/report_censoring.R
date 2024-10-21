#' @import dplyr
#' @importFrom rlang .data
censor_summary_data <- function(summary_data) {
  summary_data |>
    dplyr::mutate(
      censored = .data$n < 3
    )
}

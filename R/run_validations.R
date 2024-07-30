run_validations <- function(data, validators) {
  results <- lapply(validators, function(fun) {
    fun(data)
  })
  results <- do.call(cbind, results)
  results <- apply(results, 1, function(vec) {
    paste(na.omit(vec), collapse = "; ")
  })
  names(results) <- NULL
  data |>
    dplyr::mutate(
      "Error messages" = results,
      "Keep row?" = 1,
      "Reviewer notes" = "",
      .before = 1
    )
}

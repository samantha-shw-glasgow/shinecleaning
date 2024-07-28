run_validations <- function(data, validators) {
  results <- lapply(validators, function(fun) {
    fun(data)
  })
  results <- do.call(cbind, results)
  results <- apply(results, 1, function(vec) {
    paste(na.omit(vec), collapse = "; ")
  })
  data$errors <- results
  data[data$errors == "", ]$errors <- NA
  return(data)
}

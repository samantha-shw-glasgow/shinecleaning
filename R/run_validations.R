run_validations <- function(data, validators) {
  results <- lapply(validators, function(fun) {fun(data)})
  data$errors <- mapply(c, results, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  return(data)
}

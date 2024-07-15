run_validations <- function(data, validators) {
  data$errors <- validators[[1]](data)
  return(data)
}

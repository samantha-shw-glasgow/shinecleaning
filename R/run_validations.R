run_validations <- function(data) {
  occurrences_lookup <- table(data$dob)
  occurrences <- c(occurrences_lookup[as.character(data$dob)])
  data$errors <- ifelse(occurrences > 1, "Duplicate", NA)
  return(data)
}

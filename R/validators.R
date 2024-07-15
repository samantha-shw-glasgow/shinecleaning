check_duplicate_dob <- function(data) {
  occurrences_lookup <- table(data$dob)
  occurrences <- c(occurrences_lookup[as.character(data$dob)])
  ifelse(occurrences > 1, "Duplicate DOB", NA)
}

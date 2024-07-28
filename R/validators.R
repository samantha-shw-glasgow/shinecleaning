check_duplicate_dob <- function(data) {
  occurrences_lookup <- table(data$dob)
  occurrences <- c(occurrences_lookup[as.character(data$dob)])
  ifelse(occurrences > 1, "Duplicate DOB", NA)
}

nothing_is_wrong <- function(data) {
  rep(NA, nrow(data))
}

everything_sucks <- function(data) {
  rep("This sucks", nrow(data))
}

everything_is_fine <- function(data) {
  rep("This is fine", nrow(data))
}

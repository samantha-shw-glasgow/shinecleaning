run_validations <- function(data, validators) {
  messages <- rep("", nrow(data))
  for (validator_fn in validators) {
    result <- validator_fn(data)
    messages <- append_if_nonempty(
      messages,
      result$message
    )
  }
  data |>
    dplyr::mutate(
      "Error messages" = messages,
      "Keep row?" = 1,
      "Reviewer notes" = "",
      .before = 1
    )
}

append_if_nonempty <- function(string_1, string_2) {
  dplyr::case_when(
    string_1 == "" & string_2 == "" ~ "",
    string_1 != "" & string_2 == "" ~ string_1,
    string_1 == "" & string_2 != "" ~ string_2,
    string_1 != "" & string_2 != "" ~ paste(string_1, string_2, sep = "; "),
  )
}

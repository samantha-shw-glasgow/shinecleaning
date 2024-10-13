parse_raw_csv <- function(path) {
  df <- readr::read_csv(path,
                        col_types = readr::cols(.default = "c"),
                        show_col_types = F
  )[-1:-2, ]
  # remove unwanted top rows, re-assign col types
  drop <- NULL
  if (any(df[1, ] == colnames(df),
          na.rm = TRUE
  )) {
    drop <- c(drop, 1)
  }
  if (any(stringr::str_detect(df[2, ], "ImportId"),
          na.rm = TRUE
  )) {
    drop <- c(drop, 2)
  }
  if (length(drop) > 0) {
    df <- df[-drop, ]
  }

  df |> readr::type_convert() |> # is this a good idea?
    mutate(across(ends_with("Date"), as.character))
}

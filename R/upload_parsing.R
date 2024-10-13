parse_raw_csv <- function(path) {
  readr::read_csv(path,
                        col_types = readr::cols(.default = "c"),
                        show_col_types = F
  )[-1:-2, ]
}

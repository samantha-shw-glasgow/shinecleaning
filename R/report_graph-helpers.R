#' Print multiple graphs across chunks
#'
#' `print_first_graph` prints the first graph and returns the remaining in a list
#' `print_rest_of_graphs` prints the rest
#'
#' @param graph_list List of graphs to print
#'
#' @return `print_first_graph` invisibly returns list of graphs with first dropped
print_first_graph <- function(graph_list) {
  print(graph_list[[1]])

  graph_list[[1]] <- NULL

  invisible(graph_list)
}

#' @rdname print_first_graph

print_rest_of_graphs <- function(graph_list) {
  purrr::walk(graph_list, print)
}


#' Standard categorical fill scale
#'
#' @param ... Other arguments passed to `scale_fill_manual`
#'
scale_fill_hbsc <- function(...) {
  primary_colour <- "#4770b7"
  # secondary_colour <- "#016bb2"
  # main_colour <- "#333333"
  global_all_pupils_colour <- "#37474f"
  global_girls_colour <- "#88cbec"
  global_boys_colour <- "#4d648d"
  global_s2_colour <- "#548235"
  global_s4_colour <- "#C5E0B4"
  global_expected_colour <- "#4770b7"
  global_elevated_colour <- "#ee9457"
  global_difficulties_colour <- "#a5a5a5"
  global_girls_scotland_avg_colour <- "#008000"
  global_boys_scotland_avg_colour <- "#fb1e20"

  ggplot2::scale_fill_manual(
    values = c(
      "Girls" = global_girls_colour,
      "Boys" = global_boys_colour,
      "S2" = global_s2_colour,
      "S4" = global_s4_colour,
      "All pupils" = primary_colour,
      "All" = global_all_pupils_colour,
      "Elevated" = global_elevated_colour,
      "As expected" = global_expected_colour,
      "Difficulties" = global_difficulties_colour,
      "Borderline" = global_elevated_colour,
      "Borderline or Difficulties" = global_elevated_colour,
      "1" = primary_colour,
      "Boys (Scotland)" = global_boys_scotland_avg_colour,
      "Girls (Scotland)" = global_girls_scotland_avg_colour,
      "Prefer not to say" = global_difficulties_colour,
      "Yes" = global_expected_colour,
      "No" = global_elevated_colour
    ),
    ...
  )
}

#' Standard categorical fill scale
#'
#' @param ... Other arguments passed to `scale_fill_manual`
#'
scale_colour_hbsc <- function(...) {
  primary_colour <- "#4770b7"
  # secondary_colour <- "#016bb2"
  # main_colour <- "#333333"
  global_all_pupils_colour <- "#37474f"
  global_girls_colour <- "#88cbec"
  global_boys_colour <- "#4d648d"
  global_s2_colour <- "#548235"
  global_s4_colour <- "#C5E0B4"
  global_expected_colour <- "#4770b7"
  global_elevated_colour <- "#ee9457"
  global_difficulties_colour <- "#a5a5a5"
  global_girls_scotland_avg_colour <- "#008000"
  global_boys_scotland_avg_colour <- "#fb1e20"

  ggplot2::scale_colour_manual(
    values = c(
      "Girls" = global_girls_colour,
      "Boys" = global_boys_colour,
      "S2" = global_s2_colour,
      "S4" = global_s4_colour,
      "All pupils" = primary_colour,
      "All" = global_all_pupils_colour,
      "Elevated" = global_elevated_colour,
      "As expected" = global_expected_colour,
      "Difficulties" = global_difficulties_colour,
      "Borderline or Difficulties" = global_elevated_colour,
      "Borderline" = global_elevated_colour,
      "1" = primary_colour,
      "Boys (Scotland)" = global_boys_scotland_avg_colour,
      "Girls (Scotland)" = global_girls_scotland_avg_colour
    ),
    ...
  )
}

#' Thinner geom_bar
#'
#' @param width Width of bar (default 0.5)
#' @param ... Other arguments to pass to `geom_bar`

geom_bar_t <- function(..., width = 0.7) {
  ggplot2::geom_bar(..., width = width)
}

#' Find the mean of only the numeric entries, no warnings
#'
#' @param x A variable with mixed data types
#'
#' @return The mean of all numeric variables
quiet_means <- function(x) {

  mean(valid_numbers(x), na.rm = TRUE)

}

#' Return only numeric values, no warnings
#'
#' @param x A variable with mixed data types
#'
#' @return A vector of only valid numbers (as numeric)
valid_numbers <- function(x) {

  purrr::quietly(as.numeric)(x)$result

}

#' Return count of non-NA values
#'
#' @param x A variable with potentially NA values
#' @return Count of non-NA values
how_many_valid <- function(x) {

  sum(!is.na(x))

}

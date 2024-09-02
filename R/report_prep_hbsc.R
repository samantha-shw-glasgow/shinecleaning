#' reshape hbsc summary data
#'
#' @param dat
#' @param create_cols copy existing p7, s2 and s4 columns to fill missing years
#'
#' @return long-format dataframe
#' @export
#'
#' @examples
prep_hbsc <- function(dat = hbsc_scotland, create_cols = FALSE) {

  if (create_cols) {
    dat = dat |>
      mutate(
        p6_boys = p7_boys,
        p6_girls = p7_girls,
        s1_boys = s2_boys,
        s1_girls = s2_girls,
        s3_boys = s4_boys,
        s3_girls = s4_girls,
        s5_boys = s4_boys,
        s5_girls = s4_girls,
        s6_boys = s4_boys,
        s6_girls = s4_girls,
      )
    }

  dat |>
    mutate(
      fields2 = case_match(
        fields,
        "sch1_1" ~ "sch1_I like it a lot",
        "sch1_2" ~ "sch1_I like it a bit",
        "sch1_3" ~ "sch1_I don't like it very much",
        "sch1_4" ~ "sch1_I don't like it at all",
        "sch2_1" ~ "sch2_Not at all",
        "sch2_2" ~ "sch2_A little",
        "sch2_3" ~ "sch2_Some",
        "sch2_4" ~ "sch2_A lot",
        "sch3_1" ~ "sch3_Never",
        "sch3_2" ~ "sch3_Hardly ever",
        "sch3_3" ~ "sch3_Sometimes",
        "sch3_4" ~ "sch3_Often",
        "sch3_5" ~ "sch3_Always",
        .default = fields),
      q = str_split_i(fields2, "_", 1),
      level = str_split_i(fields2, "_", 2),
    ) |>
    select(-fields2) |>
    pivot_longer(cols = -c(fields, q, level),
               values_to = "prop",
               names_to = c("class", "gender"),
               names_sep = "_") |>
    mutate(
      class = str_to_upper(class),
      gender = case_match(
        gender,
        "boys" ~ "Boy",
        "girls" ~ "Girl"
      )
    )
}

# get the proportion by gender for provided response
get_hbsc_prop <- function(..., classes, response, var = NULL) {

  if (is.null(var)) {
    var = unique(hbsc_scotland_modified$q[hbsc_scotland_modified$level == response])
    if (length(var) > 1) {
      stop("Multiple variables found with response ", response, ". Please specify `var`.")
      }
  }

  data = hbsc_scotland_modified |>
    filter(level %in% response,
           q %in% var,
           class %in% classes) |>
    mutate(prop = prop /100,
           gender = case_match(gender,
                               "Boy" ~ "Boys (Scotland)",
                               "Girl" ~ "Girls (Scotland)"),
           )

  return(data)

}

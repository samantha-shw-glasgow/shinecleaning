#' Produce table of % of sts with elevatved or expected mm scores
#'
#'
#' @param data The dataframe of valid responses
#' @param .censor `TRUE`/`FALSE` - apply censoring rules (must be `TRUE` in output reports)
#' @param .split Split columns by gender x class
#' @param classes Vector names of classes
#' @param genders Vector names of genders
#'
#' @return dataframe
#' @export
#'
#' @examples

share_elevated <- function(data, .split, .censor = TRUE, classes = "All", genders = c("Boy", "Girl")) {

  clean_dat <- data |>
    dplyr::summarise(
      elevated = sum(mme_cat %in% c("Elevated"), na.rm = TRUE),
      expected = sum(mme_cat %in% c("Expected"), na.rm = TRUE),
      denom = sum(!is.na(mme_cat))
    ) |>
    dplyr::mutate(
      gender = "All",
      class = "All")

  if (.split) {
    split_dat <- data |>
      dplyr::group_by(gender, class) |>
      dplyr::summarise(
        elevated = sum(mme_cat %in% c("Elevated"), na.rm = TRUE),
        expected = sum(mme_cat %in% c("Expected"), na.rm = TRUE),
        denom = sum(!is.na(mme_cat))
      ) |>
      dplyr::filter(class %in% classes,
                    gender %in% genders) |>
      ungroup()

    clean_dat <- dplyr::bind_rows(clean_dat, split_dat)
  }

  graph_dat <- clean_dat |>
    mutate(censored = if_else(denom < 3 & .censor, 1, 0)) |>
    pivot_longer(-c(denom, censored, gender, class),
                 names_to = "var",
                 values_to = "n") |>
    mutate(prop = if_else(censored == 1, 1, n / denom),
           var = case_when(
             var == "elevated" ~ "Elevated",
             var == "expected" ~ "As expected"
           )
    ) |>
    select(gender, class, var, n, denom, prop, censored)

  return(graph_dat)
}

#' Produce bar graph of % of sts with elevatved or expected mm scores
#'
#' @param graph_data The output of `share_elevated`.
#'
#' @return
#' @export
#'
#' @examples
bar_share_elevated <- function(graph_data) {
  graph_dat <- graph_data |>
    mutate(
      x_lab = if_else(
        class == "All" & gender == "All",
        "All",
        stringr::str_c(class, " ", gender, "s")
      ) |> forcats::fct_relevel("All", after = Inf),
      bar_lab_main = if_else(
        censored == 1,
        "*",
        scales::percent(prop, suffix = "%", accuracy = 1)
      )
    )

  ggplot(data = graph_dat,
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_hbsc(name = "") +
    scale_y_continuous("", labels = scales::percent) +
    geom_text(aes(label = bar_lab_main),
              colour = "black",
              position = position_stack(vjust = 0.5),
              size = 4) +
    coord_cartesian(clip = "off") +
    theme(legend.justification.right = "top",
          plot.margin = unit(c(0.8, 1, 0.5, 0), "cm"),
          plot.caption = element_text(
            hjust = 1,
            size = 10,
            face = "italic"
          ),
          axis.title.x = element_blank()) +
    labs(caption = if_else(any(graph_dat$censored == 1), "* Numbers too low to show", ""),
         title = paste(stringr::str_flatten_comma(classes, " and "), "pupils"))
}

#' Produce table of % of sts with elevatved or expected mm scores
#'
#'
#' @param data The dataframe of valid responses
#' @param outcome The variable to graph
#' @param levels Levels of the variable (in ascending order)
#' @param .split Split columns by gender x class
#' @param classes Vector names of classes
#' @param genders Vector names of genders
#'
#' @return dataframe
#' @export
#'

share_elevated <-
  function(data,
           outcome,
           levels = c("As expected", "Elevated"),
           .split = TRUE,
           classes = "All",
           genders = c("Boy", "Girl")) {
    clean_dat <- purrr::map(levels, ~ data |>
      dplyr::summarise("{.x}" := sum({{ outcome }} %in% .x))) |>
      purrr::reduce(dplyr::bind_cols) |>
      dplyr::mutate(
        denom = sum(dplyr::c_across(dplyr::everything())),
        gender = "All",
        class = "All"
      )

    if (.split) {
      split_dat <-
        purrr::map(classes, \(concat_class) {
          purrr::map(
            levels,
            \(inc_level) data |>
              dplyr::filter(.data$class %in% concat_class, .data$gender %in% genders) |>
              dplyr::summarise(
                "{inc_level}" := sum({{ outcome }} %in% inc_level),
                class = stringr::str_flatten(concat_class, collapse = ", ", last = " and "),
                .by = "gender"
              )
          ) |>
            purrr::reduce(dplyr::left_join, by = dplyr::join_by("gender", "class")) |>
            dplyr::mutate(
              denom = sum(dplyr::c_across(dplyr::where(is.numeric))),
              .by = c("gender", "class")
            )
        }) |>
        purrr::reduce(dplyr::bind_rows) |>
        dplyr::arrange(class)

      clean_dat <- dplyr::bind_rows(clean_dat, split_dat)
    }

    graph_data <- clean_dat |>
      tidyr::pivot_longer(-c("denom", "gender", "class"),
        names_to = "var",
        values_to = "n"
      ) |>
      dplyr::mutate(
        prop = .data$n / .data$denom,
        var = factor(.data$var, levels = levels) |> forcats::fct_rev()
      ) |>
      dplyr::select("gender", "class", "var", "n", "denom", "prop") |>
      dplyr::arrange(.data$gender, .data$class, .data$var)

    return(graph_data)
  }

#' Produce bar graph of % of sts with elevatved or expected mm scores
#'
#' @param graph_data The output of `share_elevated`.
#'
#' @import ggplot2
#'
#' @return A ggplot2 graph
#' @export
#'
bar_share_elevated <- function(graph_data) {
  graph_dat <- graph_data |>
    dplyr::mutate(
      prop = dplyr::if_else(.data$censored, 1, .data$prop),
      x_lab = dplyr::if_else(
        .data$class == "All" & .data$gender == "All",
        "All",
        stringr::str_c(.data$class, " ", .data$gender)
      ) |> forcats::fct_relevel("All", after = Inf),
      bar_lab_main = dplyr::case_when(
        .data$censored ~ "*",
        .data$prop == 0 ~ "",
        .default = scales::percent(.data$prop, suffix = "%", accuracy = 1)
      )
    )

  lab_length <- max(stringr::str_length(graph_dat$x_lab))
  n_labs <- length(unique(graph_dat$x_lab))

  gg_out <- ggplot(
    data = graph_dat,
    aes(x = .data$x_lab, y = .data$prop, fill = .data$var)
  ) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_hbsc(name = "") +
    scale_y_continuous("",
                       labels = scales::percent,
                       limits = c(0, 1),
                       expand = expansion(add = 0)
                       ) +
    geom_text(aes(label = .data$bar_lab_main),
      colour = "black",
      position = position_fill(vjust = 0.5),
      size = dplyr::if_else(length(unique(graph_data$class)) > 3, 2.5, 3)
    ) +
    coord_cartesian(clip = "off") +
    theme(
      legend.justification.right = "top",
      plot.margin = unit(c(0.8, 1, 0.5, 0), "cm"),
      plot.caption = element_text(
        hjust = 1,
        size = 10,
        face = "italic"
      ),
      axis.title.x = element_blank()
    ) +
    labs(caption = dplyr::if_else(any(graph_dat$censored),
      "* Numbers too low to show",
      ""
    ))

  if ((lab_length * n_labs) > 60) {
    gg_out +
      theme(axis.text.x = element_text(angle = 315, hjust = 0))
  } else {
    gg_out
  }
}

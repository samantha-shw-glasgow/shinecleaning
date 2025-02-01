#' Proportions in each subgroup with elevated results across multiple vars
#'
#' @param data Prepared input data
#' @param varlist List of variable labels, with names corresponding to columns
#' @param levels Levels to sum over
#' @param .split Split by gender/class
#' @param classes Vector/list of classes, nested by clusters
#' @param genders Vector of genders
#'
#' @return A table to plot
#' @export
share_elevated_multiple <-
  function(data,
           varlist,
           levels = c("As expected", "Elevated"),
           .split = TRUE,
           classes = "All",
           genders = c("Boys", "Girls")) {



    clean_dat <- purrr::map(levels, \(level) {
      data |>
        tidyr::pivot_longer(dplyr::any_of(names(varlist)), names_to = "var", values_to = "val") |>
        dplyr::filter(.data$val %in% levels) |>
        dplyr::summarise(val = sum(.data$val %in% level), .by = c("var")) |>
        dplyr::mutate(level = level,
               class = "All",
               gender = "All",
               var = paste(varlist[.data$var]))
    }) |>
      purrr::reduce(dplyr::bind_rows) |>
      dplyr::select("gender", "class", "var", "level", n = "val") |>
      dplyr::arrange(.data$gender, .data$class, .data$var) |>
      dplyr::mutate(denom = sum(.data$n), .by = c("gender", "class", "var")) |>
      dplyr::mutate(prop = .data$n / .data$denom,
             level = factor(.data$level, levels = levels)) |>
      list()

    if (.split) {
      split_dat <-
        purrr::map(classes, \(concat_class) {
          class_summary <-
          purrr::map(levels, \(level) {
              data |>
              dplyr::filter(.data$class %in% concat_class, .data$gender %in% genders) |>
              tidyr::pivot_longer(dplyr::any_of(names(varlist)), names_to = "var", values_to = "val") |>
              dplyr::filter(.data$val == level) |>
              dplyr::summarise(n = dplyr::n(), .by = c("gender", "val", "var")) |>
              dplyr::mutate(
                level = level,
                class = stringr::str_flatten(concat_class, collapse = ", ", last = " and "),
                var = paste(.data$gender, varlist[.data$var], sep = "-")
              )

          }) |>
            purrr::reduce(dplyr::bind_rows) |>
            dplyr::select("gender", "class", "var", "level", "n") |>
            dplyr::arrange(.data$gender, .data$class, .data$var) |>
            dplyr::mutate(denom = sum(.data$n), .by = c("gender", "class", "var")) |>
            dplyr::mutate(prop = .data$n / .data$denom,
                   level = factor(.data$level, levels = levels))

            if (nrow(class_summary) == 0) {
              NULL
            } else {
              class_summary
            }

        })

      clean_dat <- c(split_dat, clean_dat) |> purrr::compact()
    }


    return(clean_dat)
  }

#' Produce bar graph of multiple  % of sts with elevatved or expected scores
#'
#' @param graph_data The output of `share_elevated_multiple`.
#'
#' @import ggplot2
#'
#' @return A ggplot2 graph
#' @export
#'
bar_share_elevated_multiple <- function(graph_data) {

  class <- unique(graph_data$class)

  graph_dat <- graph_data |>
    dplyr::mutate(
      x_lab = .data$var,
      bar_lab_main = dplyr::case_when(
        censored ~ "*",
        prop == 0 ~ "",
        .default = scales::percent(.data$prop, suffix = "%", accuracy = 1)
      )
    )

  lab_length <- max(stringr::str_length(graph_dat$x_lab))
  n_labs <- length(unique(graph_dat$x_lab))

  gg_out <- ggplot(
    data = graph_dat,
    aes(x = .data$x_lab, y = .data$prop, fill = .data$level)
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
              size = 4
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
    ),
      title = paste(class, "pupils")
    )

  if ((lab_length * n_labs) > 60) {
    gg_out +
      theme(axis.text.x = element_text(angle = 315, hjust = 0))
  } else {
    gg_out
  }
}

#' Produce table of % of sts with elevatved or expected mm scores
#'
#'
#' @param data The dataframe of valid responses
#' @param outcome The variable to graph
#' @param levels Levels of the variable (in ascending order)
#' @param .censor `TRUE`/`FALSE` - apply censoring rules (must be `TRUE` in output reports)
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
           .censor = TRUE,
           classes = "All",
           genders = c("Boy", "Girl")) {
    clean_dat <- map(levels, ~ data |>
      summarise("{.x}" := sum({{ outcome }} %in% .x))) |>
      reduce(bind_cols) |>
      mutate(
        denom = sum(c_across(everything())),
        gender = "All",
        class = "All"
      )

    if (.split) {
      split_dat <-
        map(classes, \(concat_class) {
          map(
            levels,
            \(inc_level) data |>
              dplyr::filter(class %in% concat_class, gender %in% genders) |>
              summarise(
                "{inc_level}" := sum({{ outcome }} %in% inc_level),
                class = str_flatten(concat_class, collapse = ", ", last = " and "),
                .by = "gender"
              )
          ) |>
            reduce(left_join, by = join_by(gender, class)) |>
            mutate(
              denom = sum(c_across(where(is.numeric))),
              .by = c("gender", "class")
            )
        }) |>
        reduce(bind_rows) |>
        arrange(class)

      clean_dat <- dplyr::bind_rows(clean_dat, split_dat)
    }

    graph_data <- clean_dat |>
      mutate(censored = if_else(denom < 3 & .censor, 1, 0)) |>
      pivot_longer(-c(denom, censored, gender, class),
        names_to = "var",
        values_to = "n"
      ) |>
      mutate(
        prop = if_else(censored == 1, 1, n / denom),
        var = factor(var, levels = levels) |> fct_rev()
      ) |>
      select(gender, class, var, n, denom, prop, censored) |>
      arrange(gender, class, var)

    return(graph_data)
  }

#' Produce bar graph of % of sts with elevatved or expected mm scores
#'
#' @param graph_data The output of `share_elevated`.
#'
#' @return A ggplot2 graph
#' @export
#'
bar_share_elevated <- function(graph_data) {
  graph_dat <- graph_data |>
    mutate(
      x_lab = if_else(
        class == "All" & gender == "All",
        "All",
        stringr::str_c(class, " ", gender)
      ) |> forcats::fct_relevel("All", after = Inf),
      bar_lab_main = if_else(
        censored == 1,
        "*",
        scales::percent(prop, suffix = "%", accuracy = 1)
      )
    )

  lab_length <- max(str_length(graph_dat$x_lab))
  n_labs <- length(unique(graph_dat$x_lab))

  gg_out <- ggplot(
    data = graph_dat,
    aes(x = x_lab, y = prop, fill = var)
  ) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_hbsc(name = "") +
    scale_y_continuous("", labels = scales::percent, limits = c(0, 1)) +
    geom_text(aes(label = bar_lab_main),
      colour = "black",
      position = position_stack(vjust = 0.5),
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
    labs(caption = if_else(any(graph_dat$censored == 1),
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

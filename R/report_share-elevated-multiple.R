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



    clean_dat <- map(levels, \(level) {
      data |>
        pivot_longer(any_of(names(varlist)), names_to = "var", values_to = "val") |>
        filter(val %in% levels) |>
        summarise(val = sum(val %in% level), .by = c("var")) |>
        mutate(level = level,
               class = "All",
               gender = "All",
               var = paste(varlist[var]))
    }) |>
      reduce(bind_rows) |>
      select(gender, class, var, level, n = val) |>
      arrange(gender, class, var) |>
      mutate(denom = sum(n), .by = c("gender", "class", "var")) |>
      mutate(prop = n / denom,
             level = factor(level, levels = levels)) |>
      list()

    if (.split) {
      split_dat <-
        map(classes, \(concat_class) {
          class_summary <-
          map(levels, \(level) {
              data |>
              filter(class %in% concat_class, gender %in% genders) |>
              pivot_longer(any_of(names(varlist)), names_to = "var", values_to = "val") |>
              filter(val == level) |>
              summarise(n = n(), .by = c("gender", "val", "var")) |>
              mutate(
                level = level,
                class = str_flatten(concat_class, collapse = ", ", last = " and "),
                var = paste(gender, varlist[var], sep = "-")
              )

          }) |>
            reduce(bind_rows) |>
            select(gender, class, var, level, n) |>
            arrange(gender, class, var) |>
            mutate(denom = sum(n), .by = c("gender", "class", "var")) |>
            mutate(prop = n / denom,
                   level = factor(level, levels = levels))

            if (nrow(class_summary) == 0) {
              NULL
            } else {
              class_summary
            }

        })

      clean_dat <- c(split_dat, clean_dat) |> compact()
    }


    return(clean_dat)
  }

#' Produce bar graph of multiple  % of sts with elevatved or expected scores
#'
#' @param graph_data The output of `share_elevated_multiple`.
#'
#' @return A ggplot2 graph
#' @export
#'
bar_share_elevated_multiple <- function(graph_data) {

  class <- unique(graph_data$class)
  genders <- unique(graph_data$gender)

  graph_dat <- graph_data |>
    mutate(
      x_lab = var,
      bar_lab_main = case_when(
        censored ~ "*",
        prop == 0 ~ "",
        .default = scales::percent(prop, suffix = "%", accuracy = 1)
      )
    )

  lab_length <- max(str_length(graph_dat$x_lab))
  n_labs <- length(unique(graph_dat$x_lab))

  gg_out <- ggplot(
    data = graph_dat,
    aes(x = x_lab, y = prop, fill = level)
  ) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_hbsc(name = "") +
    scale_y_continuous("",
                       labels = scales::percent,
                       limits = c(0, 1),
                       expand = expansion(add = 0)
                       ) +
    geom_text(aes(label = bar_lab_main),
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
    labs(caption = if_else(any(graph_dat$censored),
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

#' Bar graph of means for a single variable
#'
#' @param data Valid input data
#' @param var Variable to calculate mean of
#' @param .censor `TRUE`/`FALSE` - apply censoring rules (must be `TRUE` in output reports)
#' @param .gender_split `TRUE`/`FALSE` - split by gender when sufficient numbers of responses
#' @param limits Vector of upper/lower score limits.
#' @param ymax Upper limit of graph (deaults to `limits[2]`)
#' @param ylab Label for X axis (summary statistic, i.e. "mean")
#' @param classes Vector names of classes to filter/combine.
#'
#' @return A ggplot2 bar graph of mean of a scoring variables
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang .data
#'
summary_mean_single_var <-
  function(data,
           var,
           .censor = TRUE,
           .gender_split = TRUE,
           classes = "All") {
    subgroups <- tibble()

    if (.gender_split) {
      subgroups <-
        map(classes, \(concat_class) {
          data |>
            # Can this filter for two sets of groupings - class concat or expand?
            filter(gender %in% c("Boys", "Girls"), class %in% concat_class) |>
            summarise(
              mean_score = mean({{var}}, na.rm = TRUE),
              class = str_flatten(concat_class, collapse = ", ", last = " and "),
              .by = c(gender)
            )
        }) |>
        reduce(bind_rows) |>
        arrange(class)
    }


    all <- data |>
      mutate(class = "All", gender = if_else(.gender_split, "All", "All pupils")) |>
      summarise(mean_score = mean({{var}}, na.rm = TRUE),
                .by = c(class, gender))

    bind_rows(subgroups, all) |>
      mutate(bar_lab_main = sprintf("%.1f", mean_score),
             class = fct_inorder(class))

  }

bar_mean_single <- function(summary_data, ymax, ylab = "Mean") {

  summary_data |>
    ggplot() +
    aes(x = class, y = mean_score, fill = gender) +
    geom_bar_t(stat = "identity",
               position = position_dodge(width = 0.7),
               linetype = "blank") +
    scale_x_discrete("") +
    scale_fill_hbsc(name = "") +
    theme(legend.justification.right = "top",
          legend.title = element_blank(),
          plot.margin = unit(c(0.8, 1, 0.5, 0), "cm")) +
    scale_y_continuous(ylab, expand = expansion(add = 0)) +
    geom_text(
      aes(label = .data$bar_lab_main),
      vjust = -0.5,
      colour = "black",
      position = position_dodge(width = 0.7),
      size = 4
    ) +
    coord_cartesian(ylim = c(0, ymax), clip = "off")

}

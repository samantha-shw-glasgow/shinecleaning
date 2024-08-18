bar_from_summary <- function(summary_data) {
  summary_data |>
    mutate(prop = numerator/denom) |>
    ggplot() +
    aes(x = class, y = prop, fill = gender) +
    geom_col(position = "dodge") +
    scale_fill_hbsc("") +
    xlab("") +
    scale_y_continuous("%", labels = scales::percent)+
    geom_text(aes(label = scales::percent(.data$prop, suffix="%", accuracy = 1)),
              position = position_dodge(0.9),
              vjust = -0.5,
              size = 4) +
    theme(plot.margin = unit(c(0.8, 0.5, 0.5, 1),  "cm")) +
    coord_cartesian(ylim = c(0, 1), clip = "off")
}

if (FALSE) {
  # Example usage:
  tibble(
    gender = sample(c("Girl", "Boy"), 20, TRUE),
    class = sample(c("S1", "S6"), 20, TRUE),
    answer = sample(c("Yes", "No"), 20, TRUE),
  ) |>
    create_summary(answer, "Yes") |>
    bar_from_summary()
}

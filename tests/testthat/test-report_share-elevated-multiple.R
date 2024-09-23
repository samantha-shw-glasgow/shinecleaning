test_that("share elevated - mupltiple variables", {
  set.seed(1)
  input_data <- tibble(
    gender = sample(c("Girls", "Boys"), 30, TRUE),
    class = sample(c("P6", "P7"), 30, TRUE),
    cat1 = sample(c("As expected", "Elevated"), 30, TRUE),
    cat2 = sample(c("As expected", "Elevated"), 30, TRUE)
  )

  classes <- c("P6", "P7")
  levels <-  c("As expected", "Elevated")
  varlist <- list(
    cat1 = "Variable 1",
    cat2 = "Variable 2"
  )

  subgs <- map(classes, \(inc_class) {
    map(levels, \(level) {
      input_data |>
        filter(class %in% inc_class) |>
        pivot_longer(c(cat1, cat2), names_to = "var", values_to = "val") |>
        summarise(val = sum(val %in% level),
                  .by = c("gender", "var")) |>
        mutate(
          level = level,
          class = inc_class,
          var = paste(gender, varlist[var], sep = "-")
        )
    }) |>
      reduce(bind_rows) |>
      select(gender, class, var, level, n = val) |>
      arrange(gender, class, var) |>
      mutate(denom = sum(n), .by = c("gender", "class", "var")) |>
      mutate(prop = n / denom,
             censored = 0)

  })

  all <- map(levels, \(level) {
    input_data |>
      pivot_longer(c(cat1, cat2), names_to = "var", values_to = "val") |>
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
           censored = 0)


  expected <- append(subgs, list(all))

  result <- share_elevated_multiple(
    input_data,
    varlist = varlist,
    classes = classes,
    genders = c("Boys", "Girls"),
    .split = TRUE
  )

  expect_equal(result, expected)

})


bar_share_elevated_multiple <- function(graph_data) {
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

  lab_length <- max(str_length(graph_dat$class))

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

  if (lab_length > 10) {
    gg_out +
      theme(axis.text.x = element_text(angle = 315, hjust = 0))
  } else {
    gg_out
  }
}

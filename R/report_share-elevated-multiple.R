#' Proportions in each subgroup with elevated results across multiple vars
#'
#' @param data Prepared input data
#' @param varlist List of variable labels, with names corresponding to columns
#' @param levels Levels to sum over
#' @param .split Split by gender/class
#' @param .censor Censor low variables
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
           .censor = TRUE,
           classes = "All",
           genders = c("Boy", "Girl")) {



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
             censored = 0)

    if (.split) {
      split_dat <-
        map(classes, \(concat_class) {
          map(levels, \(level) {
            data |>
              filter(class %in% concat_class) |>
              pivot_longer(any_of(names(varlist)), names_to = "var", values_to = "val") |>
              filter(val == level) |>
              summarise(n = n(),
                        .by = c("gender", "val", "var")) |>
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
                   censored = 0)

        })

      clean_dat <- append(split_dat, list(clean_dat))
    }


    return(clean_dat)
  }



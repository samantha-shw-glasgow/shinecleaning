SHINE_colors <- list(
  primary = "#2d2e91",
  secondary = "#23aae1",
  tertiary = "#4d648d",
  quaternary = "#37474f"
)

SHINE_theme <- bslib::bs_theme(
  version = 5,
  primary = SHINE_colors$primary,
  secondary = SHINE_colors$secondary,
  info = SHINE_colors$tertiary,
  base_font = bslib::font_google("Roboto"),
  "bslib-spacer" = "1rem"
) |>
  bslib::bs_add_variables(
    "h2-font-size" = "$font-size-base * 1.5",
    "h3-font-size" = "$font-size-base * 1.25",
    "headings_font_weight" = "$font-weight-bold",
    "nav-link-font-size" = "$font-size-base * 1.25 !important",
    "nav-link-font-weight" = "$font-weight-bold !important",
    "tooltip-bg" = "$info",
    "body-emphasis-color" = "$primary",
    .where = "declarations"
  ) |>
  bslib::bs_add_rules(
    list(
      ".card-body {row-gap: 0rem}",
      ".nav-link.active {font-weight: $font-weight-bold !important}"
    )
  )

layout_central_column <- function(...) {
  bslib::layout_columns(
    gap = 0,
    col_widths = bslib::breakpoints(
      lg = c(-2, 8, -2),
      xl = c(-3, 6, -3),
      xxl = c(-4, 4, -4)
    ),
    bslib::layout_columns(
      col_widths = 12,
      ...
    )
  )
}

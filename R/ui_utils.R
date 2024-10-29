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
  "bslib-spacer" = "1rem",
  "accordion-padding-y" = "0.5rem",
  "accordion-padding-x" = "0",
  "accordion-border-width" = "0",
) |>
  bslib::bs_add_variables(
    "h2-font-size" = "$font-size-base * 1.5",
    "h3-font-size" = "$font-size-base * 1.25",
    "h4-font-size" = "$font-size-base * 1.1",
    "headings_font_weight" = "$font-weight-bold",
    "nav-link-font-size" = "$font-size-base * 1.25 !important",
    "nav-link-font-weight" = "$font-weight-bold !important",
    "tooltip-bg" = "$info",
    "body-emphasis-color" = "$primary",
    .where = "declarations"
  ) |>
  bslib::bs_add_rules(
    list(
      ".accordion-title {font-weight: $font-weight-bold; color: $primary; font-size: $h3-font-size}",
      ".card-body {row-gap: 0.5rem}",
      ".nav-link.active {font-weight: $font-weight-bold !important}",
      ".help-img {max-width: 100%; height: auto; border-radius: 0.25rem; box-shadow: 0 0 0.05rem 0.05rem rgba(0, 0, 0, 0.1); padding: 0.5rem}",
      ".bslib-page-navbar {display: flex; min-height: 100vh; flex-direction: column;}",
      ".footer {position:absolute; bottom:0; color: rgba(0, 0, 0, 0.3);}",
      ".bslib-page-navbar .container-fluid {flex:1; position: relative;} "
    )
  )

layout_central_column <- function(...) {
  bslib::layout_columns(
    gap = 0,
    col_widths = bslib::breakpoints(
      xs = 12,
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

#' Make upload warning UI
#'
#' @param message character
#' @param level integer, 1-3
#'
#' @return div html object
#'
#' @keywords internal
make_warning <- function(message, level) {
  if (level == 1) {
    return(
      tags$div(
        span(
          icon("circle-exclamation"),
          message
        ),
        class = "card p-2 mx-2 text-info"
      )
    )
  }

  if (level == 2) {
    return(
      tags$div(
        span(
          icon("circle-question"),
          message
        ),
        class = "card p-2 mx-2 text-warning"
      )
    )
  }

  if (level == 3) {
    return(
      tags$div(
        span(
          icon("triangle-exclamation"),
          message
        ),
        class = "card p-2 mx-2 text-danger"
      )
    )
  }
}

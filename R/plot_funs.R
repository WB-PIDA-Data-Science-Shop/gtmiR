
#' Compute change in GTMI indicator between two years for each country
#'
#' @param data A data frame in the gtmi2025 panel format.
#' @param indicator One of "gtmi", "cgsi", "psdi", "dcei", "gtei".
#' @param from_year The starting year (2020 or 2022).
#' @param to_year The ending year (2022 or 2025).
#' @return A data frame with per-country indicator differences.
compute_gtmi_diff <- function(data, indicator, from_year, to_year) {
  data |>
    filter(year %in% c(from_year, to_year),
           !is.na(.data[[indicator]])) |>
    select(country_code, country_name, grp, year, value = all_of(indicator)) |>
    tidyr::pivot_wider(
      id_cols     = c(country_code, country_name, grp),
      names_from  = year,
      values_from = value,
      names_prefix = "value_"
    ) |>
    mutate(
      indicator  = indicator,
      from_year  = from_year,
      to_year    = to_year,
      difference = .data[[paste0("value_", to_year)]] - .data[[paste0("value_", from_year)]]
    ) |>
    arrange(desc(difference))
}



#' Plot GTMI indicator change by country, facetted by grouping tier
#'
#' @param data Output from \code{compute_gtmi_diff()} joined with any grouping columns.
#' @param grouping Unquoted column name to use for both faceting and point colour.
#'   Defaults to \code{grp}. Can also be passed as a string (e.g. from \code{pwalk}).
#' @param title Plot title string. Auto-generated from indicator and years if \code{NULL}.
#' @return A ggplot lollipop chart.
#' @export
generate_gtmi_diff_plot <- function(data, grouping = grp, title = NULL) {
  # Accept both bare names (interactive) and strings (programmatic/pwalk)
  grp_var <- if (is.character(grouping)) {
    grouping
  } else {
    rlang::as_name(rlang::ensym(grouping))
  }

  data |>
    filter(!is.na(difference)) |>
    mutate(country_name = forcats::fct_reorder(country_name, difference)) |>
    ggplot(aes(x = country_name, y = difference, color = .data[[grp_var]])) +
    geom_segment(aes(xend = country_name, y = 0, yend = difference), linewidth = 0.8) +
    geom_point(size = 2, alpha = 0.7) +
    geom_hline(
      yintercept = 0,
      linetype   = "solid",
      linewidth  = 0.5,
      alpha      = 0.75,
      color      = "lightgrey"
    ) +
    labs(
      title = title %||% paste("Change in", unique(data$indicator), ":", unique(data$from_year), "\u2192", unique(data$to_year)),
      x     = NULL,
      y     = paste("Change in", toupper(unique(data$indicator)))
    ) +
    facet_wrap(vars(.data[[grp_var]]), scales = "free_y", nrow = 2) +
    scale_color_brewer(palette = "Paired") +
    scale_y_continuous(limits = c(-0.4, 0.4), breaks = c(-0.4, -0.2, 0, 0.2, 0.4)) +
    coord_flip() +
    theme(
      strip.text      = element_text(size = 14),
      legend.position = "none"
    )
}


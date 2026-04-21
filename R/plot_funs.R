
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

#' Classify GTMI indicator values into performance groups (A–D)
#'
#' Applies the GTMI grouping tiers:
#' A = Extensive (>=0.75), B = Significant (>=0.50), C = Medium (>=0.25), D = Low (<0.25).
#'
#' Works with two data formats:
#' \itemize{
#'   \item **Panel format** (\code{groups_data}): has a \code{year} column and
#'     indicator columns (\code{gtmi}, \code{cgsi}, etc.). Creates columns
#'     \code{<indicator>_group_<year>}.
#'   \item **Diff format** (\code{gtmi_diffs}): has \code{value_<year>} columns.
#'     Creates a column \code{group_<year>}.
#' }
#'
#' @param data A data frame in either panel or diff format.
#' @param year Integer. The year to classify.
#' @param indicators Character vector of indicator column names (panel format only).
#'   Defaults to \code{c("gtmi", "cgsi", "psdi", "dcei", "gtei")}.
#' @return The input data frame with new group classification columns.
#' @export
classify_gtmi_group <- function(data,
                                year,
                                indicators = c("gtmi", "cgsi", "psdi", "dcei", "gtei")) {

  value_col <- paste0("value_", year)

  # Diff format: has value_<year> columns

  if (value_col %in% names(data)) {
    group_col <- paste0("group_", year)
    data <- data |>
      dplyr::mutate(
        !!group_col := dplyr::case_when(
          .data[[value_col]] >= 0.75 ~ "A",
          .data[[value_col]] >= 0.50 ~ "B",
          .data[[value_col]] >= 0.25 ~ "C",
          .data[[value_col]] <  0.25 ~ "D",
          TRUE                       ~ NA_character_
        )
      )
    return(data)
  }

  # Panel format: has year column and indicator columns
  for (ind in indicators) {
    group_col <- paste0(ind, "_group_", year)

    data <- data |>
      dplyr::mutate(
        !!group_col := dplyr::case_when(
          .data$year != .env$year ~ NA_character_,
          .data[[ind]] >= 0.75   ~ "A",
          .data[[ind]] >= 0.50   ~ "B",
          .data[[ind]] >= 0.25   ~ "C",
          .data[[ind]] <  0.25   ~ "D",
          TRUE                   ~ NA_character_
        )
      )
  }
  data
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


#' Plot Year-over-Year GTMI Score Change by Country
#'
#' Creates a lollipop chart of GTMI score changes between two years for all
#' countries in \code{data}, coloured by a grouping variable. Countries are
#' ordered by the magnitude of change (descending). Both the colour legend
#' and (optional) facet panels follow the order supplied in \code{group_order}.
#' Returns \code{NULL} invisibly if \code{data} contains no rows.
#'
#' Accepts a single indicator or a vector of indicators. When a vector is
#' supplied, returns a named list of plots keyed as
#' \code{<indicator>_<from_year>_<to_year>}.
#'
#' @param data A data frame from \code{\link{compute_gtmi_diff}} (optionally
#'   classified via \code{\link{classify_gtmi_group}} and joined with country
#'   metadata), containing at least \code{country_code}, \code{indicator},
#'   \code{from_year}, \code{to_year}, \code{difference}, and the column
#'   named in \code{grouping}.
#' @param indicator Character vector or single string. The indicator(s) to plot.
#' @param grouping Character string. Column name used for colour and optional
#'   faceting. Defaults to \code{"income_group"}.
#' @param title Character. Optional plot title. Auto-generated if \code{NULL}.
#'   Ignored when \code{indicator} is a vector.
#' @param subtitle Character. Optional plot subtitle.
#' @param y_limits Numeric vector of length 2. Y-axis limits.
#'   Defaults to \code{c(-0.4, 0.4)}.
#' @param group_order Character vector. Optional ordering of grouping levels
#'   for the legend and facets. Defaults to \code{NULL} (alphabetical).
#'
#' @return A ggplot object if \code{indicator} is length 1; otherwise a named
#'   list of ggplot objects.
#'
#' @examples
#' \dontrun{
#' # Single plot
#' plot_gtmi_time_trends(gtmi_classified, indicator = "gtmi", grouping = "income_group")
#'
#' # Multiple indicators
#' plots <- plot_gtmi_time_trends(
#'   gtmi_classified,
#'   indicator = c("gtmi", "cgsi", "psdi", "dcei", "gtei"),
#'   grouping = "income_group",
#'   group_order = c("High income", "Upper middle income",
#'                   "Lower middle income", "Low income")
#' )
#' purrr::iwalk(plots, ~ggsave(paste0(.y, ".png"), .x))
#' }
#' @importFrom dplyr filter mutate group_by ungroup
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot aes geom_segment geom_point geom_hline
#'   facet_wrap scale_y_continuous labs theme element_text element_blank
#' @importFrom purrr map
#' @export
plot_gtmi_time_trends <- function(
    data,
    indicator,
    grouping    = "income_group",
    title       = NULL,
    subtitle    = NULL,
    y_limits    = c(-0.4, 0.4),
    group_order = NULL
) {

  .make_plot <- function(ind, ttl = NULL) {

    plot_data <- data |>
      dplyr::filter(
        .data$indicator == ind,
        !is.na(.data$difference),
        !is.na(.data[[grouping]])
      )

    if (nrow(plot_data) == 0) {
      warning("No data for indicator: ", ind, " \u2014 skipping plot.")
      return(invisible(NULL))
    }

    from_yr <- unique(plot_data$from_year)
    to_yr   <- unique(plot_data$to_year)

    auto_title <- ttl %||% paste(
      "Change in", toupper(ind), ":",
      from_yr, "\u2192", to_yr
    )
    auto_sub <- subtitle %||% paste(
      "Score change by", gsub("_", " ", grouping)
    )

    # Optionally set factor order for grouping
    if (!is.null(group_order)) {
      plot_data <- plot_data |>
        dplyr::mutate(
          !!grouping := factor(.data[[grouping]], levels = group_order)
        )
    }

    plot_data |>
      dplyr::group_by(.data[[grouping]]) |>
      dplyr::mutate(
        country_code = forcats::fct_reorder(country_code, difference, .desc = TRUE)
      ) |>
      dplyr::ungroup() |>
      ggplot(aes(x = country_code, y = difference, color = .data[[grouping]])) +
      geom_segment(
        aes(xend = country_code, y = 0, yend = difference),
        linewidth = 0.7
      ) +
      geom_point(size = 2, alpha = 0.75) +
      geom_hline(
        yintercept = 0,
        linetype   = "solid",
        linewidth  = 0.5,
        alpha      = 0.6,
        color      = "grey60"
      ) +
      scale_color_manual(
        values = c("#00274C", "#00B2A9", "#2F65A7", "#702082"),
        name   = tools::toTitleCase(gsub("_", " ", grouping))
      ) +
      scale_y_continuous(
        limits = y_limits,
        breaks = scales::breaks_pretty()
      ) +
      labs(
        title    = auto_title,
        subtitle = auto_sub,
        x        = paste("Countries (grouped by", gsub("_", " ", grouping), ")"),
        y        = paste("Change in", toupper(ind), "Score")
      ) +
      theme(
        strip.text      = element_text(size = 12),
        legend.position = "bottom",
        axis.text.x     = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6)
      )
  }

  # If indicator is a vector, return named list of plots
  if (length(indicator) > 1) {
    plots <- purrr::map(indicator, .make_plot)
    # Build names as <indicator>_<from_year>_<to_year>
    from_yr <- unique(data$from_year)[1]
    to_yr   <- unique(data$to_year)[1]
    names(plots) <- paste0(tolower(indicator), "_", from_yr, "_", to_yr)
    return(plots)
  }

  .make_plot(indicator, ttl = title)
}
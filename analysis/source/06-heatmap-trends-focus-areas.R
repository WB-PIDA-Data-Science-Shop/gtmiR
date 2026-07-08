# This does a heatmap analysis to grp, region, income group country level heatmaps

# The objective is to unpack the indicators scores are across groups to unpack patterns
# Comparing 2022 and 2025 to find interesting global findings in terms of
# delta/change heatmap showing the shift between 2022 and 2025

# 4 Focus areas only

# set-up -----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(stringr)
library(readr)
library(purrr)
library(readxl)
library(ggrepel)

theme_set(
  theme_minimal() +
    theme(
      text = element_text(size = 16, family = "Segoe UI Semibold"),
      axis.text.x = element_text(size = 14, hjust = .5),
      axis.text.y = element_text(size = 16),
      plot.title = element_text(size = 22, face = "bold"),
      plot.subtitle = element_text(size = 16),
      plot.background = element_blank(),
      plot.caption = element_text(hjust = 0, size = 12),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    )
)

ggsave <- partial(
  ggplot2::ggsave,
  bg = "white",
  width = 14,
  height = 8
)


# data-load ----------------------------------------------------------------

devtools::load_all()
library(cliaretl)

vdem_data       <- cliaretl::vdem_data
loggdp_data     <- cliaretl::closeness_to_frontier_static
dictionary      <- gtmi_indicator_metadata
raw_panel       <- gtmi_indicators
gtmi_data       <- gtmi_data


# dictionary-driven setup -------------------------------------------------
# Group memberships and normalization maxes are derived from gtmi_indicator_metadata.
# Standalones (external_index) are excluded.
# Only indicators with 2-year or 3-year longitudinal feasibility are retained
# (both waves available for the 2022–2025 comparison).

dict_feasible <- dictionary |>
  dplyr::filter(
    longitudinal_feasibility %in% c("2-year", "3-year"),
    data_source == "survey"
  )

# Max score from dictionary: progressive_levels for progressive, 1 for binary
indicator_max <- dict_feasible |>
  dplyr::mutate(
    max_score = dplyr::case_when(
      scoring_method == "progressive" ~ as.numeric(progressive_levels),
      scoring_method == "binary"      ~ 1,
      TRUE                            ~ NA_real_
    )
  ) |>
  dplyr::filter(!is.na(max_score)) |>
  dplyr::select(indicator, max_score) |>
  tibble::deframe()

# Pillar-based groups: all feasible survey indicators assigned to a pillar
pillar_groups <- dict_feasible |>
  dplyr::filter(!is.na(pillar), indicator %in% names(indicator_max)) |>
  dplyr::group_by(pillar) |>
  dplyr::summarise(indicators = list(indicator), .groups = "drop") |>
  tibble::deframe()

# Row order for heatmap: the 4 official pillars only
indicator_groups <- pillar_groups

group_labels <- c(
  cgsi = "Core Government Systems Index (CGSI)",
  psdi = "Public Service Delivery Index (PSDI)",
  dcei = "Digital Citizen Engagement Index (DCEI)",
  gtei = "GovTech Enablers Index(GTEI)"
)


# panel-data --------------------------------------------------------------

grp_lookup <- gtmi_data |>
  filter(year == 2025) |>
  select(country_code, grp)

country_class <- cliaretl::wb_income_and_region |>
  select(country_code, region, income_group) |>
  mutate(
    region = case_when(
      region == "East Asia & Pacific"                                ~ "EAP",
      region == "Europe & Central Asia"                             ~ "ECA",
      region == "Latin America & Caribbean"                         ~ "LAC",
      region == "Middle East, North Africa, Afghanistan & Pakistan" ~ "MENAAP",
      region == "South Asia"                                        ~ "SAR",
      region == "Sub-Saharan Africa"                                ~ "SSA",
      region == "North America"                                     ~ "NAM",
      TRUE ~ region
    )
  )


# normalize-and-score -----------------------------------------------------

heatmap_panel <- raw_panel |>
  filter(year %in% c(2022, 2025), !wave_dropout) |>
  select(country_code, year, all_of(names(indicator_max))) |>
  mutate(
    across(
      all_of(names(indicator_max)),
      ~ . / indicator_max[cur_column()]
    )
  ) |>
  mutate(
    cgsi = rowMeans(pick(all_of(pillar_groups$cgsi)), na.rm = TRUE),
    psdi = rowMeans(pick(all_of(pillar_groups$psdi)), na.rm = TRUE),
    dcei = rowMeans(pick(all_of(pillar_groups$dcei)), na.rm = TRUE),
    gtei = rowMeans(pick(all_of(pillar_groups$gtei)), na.rm = TRUE)
  ) |>
  select(country_code, year, all_of(names(group_labels))) |>
  left_join(grp_lookup,    by = "country_code") |>
  left_join(country_class, by = "country_code") |>
  filter(!is.na(region), !is.na(grp), !is.na(income_group)) |>
  # after
  mutate(
    grp          = factor(grp, levels = c("A", "B", "C", "D")),
    income_group = factor(
      income_group,
      levels = c("High income", "Upper middle income", "Lower middle income", "Low income")
    )
  )


# ── helpers ─────────────────────────────────────────────────────────────────

#' Prepare and normalise plot data for level or delta heatmap
prep_heatmap_data <- function(data, group_by_col, type, year_val) {
  grp_vars <- names(group_labels)

  if (type == "level") {
    data |>
      dplyr::filter(year == year_val) |>
      dplyr::group_by(across(all_of(group_by_col))) |>
      dplyr::summarise(across(all_of(grp_vars), ~ mean(., na.rm = TRUE)), .groups = "drop") |>
      tidyr::pivot_longer(all_of(grp_vars), names_to = "indicator_group", values_to = "value") |>
      dplyr::mutate(
        label           = scales::number(value, accuracy = 0.01),
        indicator_group = factor(indicator_group, levels = grp_vars, labels = unname(group_labels)),
        # White text on dark fills, near-black on pale fills
        text_col        = dplyr::if_else(value > 0.55, "white", "grey15")
      )

  } else {
    out <- data |>
      dplyr::group_by(across(all_of(c(group_by_col, "year")))) |>
      dplyr::summarise(across(all_of(grp_vars), ~ mean(., na.rm = TRUE)), .groups = "drop") |>
      tidyr::pivot_longer(all_of(grp_vars), names_to = "indicator_group", values_to = "score") |>
      tidyr::pivot_wider(names_from = year, values_from = score, names_prefix = "yr_") |>
      dplyr::mutate(
        value           = (yr_2025 - yr_2022) * 100,
        label           = scales::number(value, accuracy = 0.1, suffix = " pp"),
        indicator_group = factor(indicator_group, levels = grp_vars, labels = unname(group_labels))
      )

    max_abs <- max(abs(out$value), na.rm = TRUE)
    out |>
      dplyr::mutate(
        # Adaptive contrast: white for saturated bubbles, dark for near-zero
        text_col = dplyr::if_else(abs(value) / max_abs > 0.45, "white", "grey15")
      )
  }
}


#' Build a type-appropriate fill scale
build_fill_scale <- function(type, plot_data) {
  if (type == "level") {
    ggplot2::scale_fill_gradientn(
      colours = c("#deebf7", "#6baed6", "#2171b5", "#084594"),
      limits  = c(0, 1),
      labels  = scales::label_number(accuracy = 0.1),
      name    = "Mean score\n(0 – 1)"
    )
  } else {
    max_abs <- max(abs(plot_data$value), na.rm = TRUE)
    ggplot2::scale_fill_gradientn(
      colours = c("#b2182b", "#ef8a62", "#fddbc7", "white", "#d9f0d3", "#5aae61", "#1b7837"),
      limits  = c(-max_abs, max_abs),
      labels  = scales::label_number(accuracy = 1, suffix = " pp"),
      name    = "Change\n(pp, 2022→2025)"
    )
  }
}


#' Alternating row bands as a list of annotate() layers (works on discrete y)
row_shade_layers <- function(plot_data, fill = "grey96") {
  n_lvls    <- nlevels(plot_data$indicator_group)
  shade_idx <- seq(2, n_lvls, by = 2)          # shade every other row
  purrr::map(
    shade_idx,
    ~ ggplot2::annotate(
      "rect",
      xmin = -Inf, xmax = Inf,
      ymin = .x - 0.46, ymax = .x + 0.46,
      fill = fill, color = NA
    )
  )
}


# ── main function ────────────────────────────────────────────────────────────

make_heatmap <- function(data,
                         group_by_col,
                         type     = c("level", "delta"),
                         year_val = 2025) {
  type      <- match.arg(type)
  plot_data <- prep_heatmap_data(data, group_by_col, type, year_val)

  titles <- list(
    level = list(
      title    = glue::glue("Indicator group scores by {group_by_col} \u2014 {year_val}"),
      subtitle = "Mean normalised score (0\u20131); bubble area \u221d value"
    ),
    delta = list(
      title    = glue::glue("Change in indicator group scores by {group_by_col} \u2014 2022 to 2025"),
      subtitle = "Percentage-point change 2022\u21922025; bubble area \u221d absolute change"
    )
  )

  ggplot2::ggplot(plot_data,
                  ggplot2::aes(x = .data[[group_by_col]], y = indicator_group)) +
    # ① Alternating row shading (must precede geom_point so bubbles sit on top)
    row_shade_layers(plot_data) +
    # ② Bubbles
    ggplot2::geom_point(
      ggplot2::aes(size = abs(value), fill = value),
      shape = 21, color = "white", stroke = 0.6
    ) +
    # ③ Labels — adaptive contrast (white on dark, near-black on pale)
    ggplot2::geom_text(
      ggplot2::aes(label = label, color = text_col),
      size = 3.5, fontface = "bold"
    ) +
    ggplot2::scale_color_identity() +
    # ④ Larger minimum so small-value bubbles still carry a readable label
    ggplot2::scale_size_continuous(range = c(10, 30), guide = "none") +
    build_fill_scale(type, plot_data) +
    ggplot2::labs(
      title    = titles[[type]]$title,
      subtitle = titles[[type]]$subtitle,
      x = NULL, y = NULL
    ) +
    ggplot2::theme(
      axis.text.x        = ggplot2::element_text(size = 14, face = "bold"),
      axis.text.y        = ggplot2::element_text(size = 13, hjust = 1),
      legend.position    = "right",
      legend.title       = ggplot2::element_text(size = 11),
      legend.text        = ggplot2::element_text(size = 10),
      # panel.grid.major   = ggplot2::element_blank(),   # row bands replace the grid
      plot.subtitle      = ggplot2::element_text(
        size = 13, color = "grey45",
        margin = ggplot2::margin(b = 10)
      ),
      plot.title         = ggplot2::element_text(
        margin = ggplot2::margin(b = 2)
      )
    )
}
# batch-heatmaps ----------------------------------------------------------

color_versions <- c("grp", "region", "income_group")

purrr::walk(color_versions, function(col_by) {

  out_dir <- file.path("analysis/figs/heatmaps", col_by)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  p_level <- make_heatmap(heatmap_panel, col_by, type = "level", year_val = 2025)
  ggsave(
    file.path(out_dir, glue::glue("fa_heatmap_level_{col_by}_2025.png")),
    plot = p_level
  )

  p_delta <- make_heatmap(heatmap_panel, col_by, type = "delta")
  ggsave(
    file.path(out_dir, glue::glue("fa_heatmap_delta_{col_by}_2022_2025.png")),
    plot = p_delta
  )
})
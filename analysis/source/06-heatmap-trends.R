# This does a heamap analysis to grp, region, income group country level heatmaps

# The objective is to unpack the indicators scores are across groups to unpack patterns
# Comapiring 2022 ansd 2025 to find interesting global findings in terms of 
# delta/change heatmap showing the shift between 2022 and 2025

# Groups    :
#   Interoperability       → I-1, I-2, I-3, I-4
#   Core Gov. Systems      → I-5–I-14, I-20–I-25
#   Public Sector Innov.   → I-46, I-47, I-48
#   Data Enablers          → I-34, I-37, I-38, I-39
#   Institutional Setting  → I-33, I-35, I-36
#   Digital Engagement     → I-28, I-29, I-30, I-31, I-32
  
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

# Load outcomes
library(cliaretl)

# Load outcomes of interest 
vdem_data <- cliaretl::vdem_data

loggdp_data <- cliaretl::closeness_to_frontier_static

# Load indicators

dictionary <- gtmi_indicator_metadata

raw_panel <- gtmi_indicators


# load 2025 grp data
gtmi_data <- gtmi_data

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

# Two custom cross-cutting groups (pillar = NA in dictionary)
custom_groups <- list(
  interoperability    = paste0("wb_gtmi_i_", 1:4),
  public_sector_innov = paste0("wb_gtmi_i_", c(46, 47, 48))
)

# Row order for heatmap: custom groups first, then the 4 official pillars
indicator_groups <- c(custom_groups, pillar_groups)

group_labels <- c(
  interoperability    = "Interoperability",
  public_sector_innov = "Public Sector Innov.",
  cgsi                = "Core Government Systems Index (CGSI)",
  psdi                = "Public Service Delivery (PSDI)",
  dcei                = "Digital Citizen Engagement (DCEI)",
  gtei                = "GovTech Enabling Env. (GTEI)"
)


# panel-data --------------------------------------------------------------

# grp (maturity band A–D) from gtmi_data composite index panel, 2025 wave
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
  # Normalize each indicator to 0–1 using dictionary-derived max
  mutate(
    across(
      all_of(names(indicator_max)),
      ~ . / indicator_max[cur_column()]
    )
  ) |>
  # Compute group scores as mean of normalized indicators within each group
  mutate(
    interoperability    = rowMeans(pick(all_of(custom_groups$interoperability)),    na.rm = TRUE),
    public_sector_innov = rowMeans(pick(all_of(custom_groups$public_sector_innov)), na.rm = TRUE),
    cgsi                = rowMeans(pick(all_of(pillar_groups$cgsi)),                na.rm = TRUE),
    psdi                = rowMeans(pick(all_of(pillar_groups$psdi)),                na.rm = TRUE),
    dcei                = rowMeans(pick(all_of(pillar_groups$dcei)),                na.rm = TRUE),
    gtei                = rowMeans(pick(all_of(pillar_groups$gtei)),                na.rm = TRUE)
  ) |>
  select(country_code, year, all_of(names(group_labels))) |>
  left_join(grp_lookup,    by = "country_code") |>
  left_join(country_class, by = "country_code") |>
  filter(!is.na(region), !is.na(grp), !is.na(income_group)) |>
  mutate(grp = factor(grp, levels = c("A", "B", "C", "D")))



# heatmap-function --------------------------------------------------------

make_heatmap <- function(data, group_by_col, type = c("level", "delta"), year_val = 2025) {
  type     <- match.arg(type)
  grp_vars <- names(group_labels)

  if (type == "level") {
    plot_data <- data |>
      filter(year == year_val) |>
      group_by(across(all_of(group_by_col))) |>
      summarise(across(all_of(grp_vars), ~ mean(., na.rm = TRUE)), .groups = "drop") |>
      pivot_longer(all_of(grp_vars), names_to = "indicator_group", values_to = "value") |>
      mutate(
        label           = scales::number(value, accuracy = 0.01),
        indicator_group = factor(indicator_group, levels = grp_vars, labels = unname(group_labels))
      )

    scale_fill  <- scale_fill_gradientn(
      colours = c("#f7fbff", "#2171b5", "#08306b"),
      limits  = c(0, 1),
      name    = "Mean score (0–1)"
    )
    title <- glue::glue("Indicator group scores by {group_by_col} — {year_val}")

  } else {
    # Percentage-point change: (mean_2025 - mean_2022) × 100
    plot_data <- data |>
      group_by(across(all_of(c(group_by_col, "year")))) |>
      summarise(across(all_of(grp_vars), ~ mean(., na.rm = TRUE)), .groups = "drop") |>
      pivot_longer(all_of(grp_vars), names_to = "indicator_group", values_to = "score") |>
      pivot_wider(names_from = year, values_from = score, names_prefix = "yr_") |>
      mutate(
        value           = (yr_2025 - yr_2022) * 100,
        label           = scales::number(value, accuracy = 0.1, suffix = " pp"),
        indicator_group = factor(indicator_group, levels = grp_vars, labels = unname(group_labels))
      )

    max_abs     <- max(abs(plot_data$value), na.rm = TRUE)
    scale_fill  <- scale_fill_gradientn(
      colours = c("#d73027", "white", "#1a9850"),
      limits  = c(-max_abs, max_abs),
      name    = "Change (pp, 2022→2025)"
    )
    title <- glue::glue("Change in indicator group scores by {group_by_col} — 2022 to 2025")
  }

  ggplot(plot_data,
         aes(x = .data[[group_by_col]], y = indicator_group)) +
    geom_point(aes(size = abs(value), fill = value), shape = 21, color = "white", stroke = 0.4) +
    geom_text(aes(label = label), size = 3.5, color = "white", fontface = "bold") +
    scale_size_continuous(range = c(6, 22), guide = "none") +
    scale_fill +
    labs(title = title, x = NULL, y = NULL) +
    theme(
      axis.text.x     = element_text(angle = 0, hjust = 0.5),
      legend.position = "right",
      panel.grid.major = element_line(color = "grey92")
    )
}


# batch-heatmaps ----------------------------------------------------------

color_versions <- c("grp", "region", "income_group")

purrr::walk(color_versions, function(col_by) {

  out_dir <- file.path("analysis/figs/heatmaps", col_by)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Level heatmap — 2025 scores
  p_level <- make_heatmap(heatmap_panel, col_by, type = "level", year_val = 2025)
  ggsave(
    file.path(out_dir, glue::glue("heatmap_level_{col_by}_2025.png")),
    plot = p_level
  )

  # Delta heatmap — 2022→2025 percentage-point change
  p_delta <- make_heatmap(heatmap_panel, col_by, type = "delta")
  ggsave(
    file.path(out_dir, glue::glue("heatmap_delta_{col_by}_2022_2025.png")),
    plot = p_delta
  )
})




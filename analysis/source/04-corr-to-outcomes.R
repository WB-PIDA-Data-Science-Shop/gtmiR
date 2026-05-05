# Correlation plots, region and income group
# This script brings data from cliaretl to corralate GTMI indicators with outcomes by region and income group.
# The objective is to label those countries that have informations systems and data management practices in place and see how they correlate with outcomes.



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

# Scores
groups_data <- gtmi2025 |> # Lazy data cleaned in raw-data/0.gtmi_data_cleaning.R
      filter(year == 2025) # only use 2025 data for now, as we don't have data for previous years
  
# Load data about IMS adoption
adoption <- ims_adoption

# Load outcomes
library(cliaretl)

vdem_data <- cliaretl::vdem_data


# vdem_core_v2juaccnt - Judicial accountability (`v2juaccnt`)
# vdem_core_v2stcritrecadm - V-Dem: state capacity – critical resources in administration, rescaled.
# vdem_core_v2x_execorr V-Dem: executive corruption index (reversed & rescaled).
# vdem_core_v2x_gender V-Dem: gender equality index, rescaled.
# vdem_core_v2x_pubcorr V-Dem: public sector corruption (reversed & rescaled).


loggdp_data <- cliaretl::closeness_to_frontier_static

# wdi_nygdppcapppkdp - GDP per capita, PPP (constant 2017 international $)
# log_gdp}{Natural log of GDP per capita (PPP).

# adoption-transf --------------------------------------------------------
# Merge adoption to gtmi groups data
adoption_transf <- adoption |>
  select(country_code, year, FMIS, TMIS, CMIS, EPMIS, DMIS, PIMIS) |>
  pivot_longer(
    cols = c(FMIS, TMIS, CMIS, EPMIS, DMIS, PIMIS),
    names_to = "ims_type",
    values_to = "adoption_status"
  ) |> 
  select(-year) 

# Merge with gtmi groups data and country classifications
country_class <- cliaretl::wb_income_and_region |>
  select(country_code, region, income_group) 

adoption_gtmi <- adoption_transf |>
  left_join(groups_data, by = "country_code") |>
  left_join(country_class, by = "country_code")


# outcomes-clean ---------------------------------------------------------

# V-Dem indicators filtered to 2025
vdem_clean <- vdem_data |>
  filter(year == 2024) |>
  select(
    country_code,
    vdem_core_v2juaccnt,         # Judicial accountability
    # vdem_core_v2stcritrecadm,  # State capacity – critical resources in administration
    vdem_core_v2x_execorr,       # Executive corruption index (reversed & rescaled)
    vdem_core_v2x_pubcorr        # Public sector corruption (reversed & rescaled)
  )

# GDP indicators filtered to 2025
loggdp_clean <- loggdp_data |>
  select(
    country_code,
    # wdi_nygdppcapppkd,  # GDP per capita, PPP (constant 2017 international $)
    log_gdp              # Natural log of GDP per capita (PPP)
  )





# corr-data --------------------------------------------------------------

corr_data <- adoption_gtmi |>
  left_join(vdem_clean, by = "country_code") |>
  left_join(loggdp_clean, by = "country_code") |> 
  filter(!is.na(region)) # Exclude countries without region classification


# Pivot longer for correlation plots
corr_data_long <- corr_data |>
  pivot_longer(
    cols = c(vdem_core_v2juaccnt, vdem_core_v2x_execorr, vdem_core_v2x_pubcorr, log_gdp),
    names_to = "outcome",
    values_to = "outcome_value"
  ) |>
  mutate(
    outcome = recode(outcome,
      "vdem_core_v2juaccnt"   = "Judicial accountability",
      "vdem_core_v2x_execorr" = "Executive corruption index",
      "vdem_core_v2x_pubcorr" = "Public sector corruption",
      "log_gdp"               = "Log GDP per capita (PPP)"
    ),
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

corr_label <- corr_data_long |>
  mutate(
    adoption_l = case_when(
      adoption_status == "-"   ~ "No data",
      adoption_status == "0"   ~ "Not in place",
      adoption_status == "3"   ~ "Under development",
      adoption_status == "4.5" ~ "Under development", # Partially operational (limited)
      adoption_status == "6"   ~ "Partially operational",
      adoption_status == "9"   ~ "Fully operational",
      TRUE                     ~ NA_character_
    ),
    adoption_label = factor(
      adoption_l,
      levels = c("No data", "Not in place", "Under development",
                 "Partially operational", "Fully operational")
    )
  )

# plot -------------------------------------------------------------------
library(tidyverse)
library(ggrepel)

# ── Parameters ────────────────────────────────────────────────────────────────
indices  <- c("gtmi", "dcei", "gtei", "psdi") # excluding "cgsi" because it it the correcation 
outcomes <- unique(corr_label$outcome)
color_versions <- c("region", "income_group")

adoption_colors <- c(
  "No data"              = "grey70",
  "Not in place"         = "#E41A1C",
  "Under development"    = "#FF7F00",
  "Partially operational"= "#377EB8",
  "Fully operational"    = "#4DAF4A"
)

# ── Plot function ──────────────────────────────────────────────────────────────
make_corr_plot <- function(data, index, outcome_var, color_by) {

  plot_data <- data |>
    dplyr::filter(
      outcome == outcome_var,
      !is.na(.data[[index]]),
      !is.na(outcome_value),
      adoption_label != "No data",
      !is.na(adoption_label),
      !is.na(.data[[color_by]])
    )

  ggplot(plot_data, aes(x = .data[[index]], y = outcome_value)) +
    # Points colored by region / income_group
    geom_point(
      aes(color = .data[[color_by]]),
      alpha = 0.7, size = 2
    ) +
    # Country code labels: one per country per facet, placed at mean position
    ggrepel::geom_text_repel(
      data = plot_data |>
        dplyr::group_by(country_code, adoption_label, .data[[color_by]]) |>
        dplyr::summarise(
          x_pos = mean(.data[[index]], na.rm = TRUE),
          y_pos = mean(outcome_value,  na.rm = TRUE),
          .groups = "drop"
        ),
      aes(x = x_pos, y = y_pos, label = country_code, color = .data[[color_by]]),
      size         = 2.5,
      max.overlaps = 20,
      show.legend  = FALSE
    ) +
    # One regression line per adoption_label, mapped to linetype
    geom_smooth(
      aes(linetype = adoption_label, group = adoption_label),
      method    = "lm",
      formula   = y ~ x,
      se        = FALSE,
      color     = "grey30",
      linewidth = 0.8
    ) +
    scale_linetype_manual(
      values = c(
        "Not in place"          = "dashed",
        "Under development"     = "dotdash",
        "Partially operational" = "longdash",
        "Fully operational"     = "solid"
      ),
      name = "Adoption status"
    ) +
    scale_color_manual(
      values = if (color_by == "region") {
        scales::hue_pal()(length(unique(plot_data[[color_by]])))
      } else {
        c("#00274C", "#2F65A7", "#00B2A9", "#702082")
      },
      name = tools::toTitleCase(gsub("_", " ", color_by))
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      title    = glue::glue("{toupper(index)} vs {outcome_var}"),
      subtitle = glue::glue("Lines = adoption status trend | Color = {gsub('_', ' ', color_by)}"),
      x        = toupper(index),
      y        = outcome_var,
      caption  = "One regression line per adoption label"
    ) +
    facet_wrap(~ adoption_label, scales = "free") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "right")
}

# ── Batch save ─────────────────────────────────────────────────────────────────
combos <- tidyr::expand_grid(
  index      = indices,
  outcome    = outcomes,
  color_by   = color_versions
)

output_dir <- "analysis/figs/corr/adoption"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

purrr::pwalk(combos, function(index, outcome, color_by) {
  p <- make_corr_plot(corr_label, index, outcome, color_by)

  filename <- file.path(
    output_dir,
    glue::glue("{index}_{snakecase::to_snake_case(outcome)}_{color_by}.png")
  )

  ggplot2::ggsave(
    filename = filename,
    plot     = p,
    width    = 14, height = 9, dpi = 300, bg = "white"
  )
})




# # Single plot
# p <- corr_data_long |> 
#   ggplot_corr_outcomes(x = "gtmi", group = "adoption_l")

# # All combinations, saved to disk
# all_plots <- batch_corr_plots(
#   corr_data_long,
#   output_dir = "analysis/figs/corr/adopt"
# )


# data-quality -----------------------------------------------------------

 outliers <- corr_data_long |>
  dplyr::filter(adoption_status %in% c("-", "0","4.5")) |>
  dplyr::distinct(country_name, adoption_status) |>
  dplyr::arrange(adoption_status, country_name)


# cgsi vs outcomes  ------------------------------------------------------
# Set 1: x = cgsi, y = outcome_value — one plot per outcome × color version
# Set 2: x = cgsi, y = other index   — one plot per index  × color version

# Base data for cgsi plots: one row per country (deduplicate from long format)
cgsi_base <- corr_data_long |>
  dplyr::distinct(country_code, country_name, cgsi, region, income_group,
                  outcome, outcome_value)

# ── Set 1 helper ──────────────────────────────────────────────────────────────
make_cgsi_outcome_plot <- function(data, outcome_var, color_by) {

  plot_data <- data |>
    dplyr::filter(
      outcome == outcome_var,
      !is.na(cgsi),
      !is.na(outcome_value),
      !is.na(.data[[color_by]])
    )

  ggplot(plot_data, aes(x = cgsi, y = outcome_value)) +
    geom_point(aes(color = .data[[color_by]]), alpha = 0.7, size = 2) +
    ggrepel::geom_text_repel(
      aes(label = country_code, color = .data[[color_by]]),
      size = 2.5, max.overlaps = 20, show.legend = FALSE
    ) +
    geom_smooth(
      method = "lm", formula = y ~ x,
      se = FALSE, color = "grey30", linetype = "dashed", linewidth = 0.9,
      na.rm     = TRUE
    ) +
    scale_color_manual(
      values = if (color_by == "region") {
        scales::hue_pal()(length(unique(plot_data[[color_by]])))
      } else {
        c("#00274C", "#2F65A7", "#00B2A9", "#702082")
      },
      name = tools::toTitleCase(gsub("_", " ", color_by))
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      title    = glue::glue("CGSI vs {outcome_var}"),
      subtitle = glue::glue("Color = {gsub('_', ' ', color_by)}"),
      x        = "CGSI",
      y        = outcome_var,
      caption  = "Overall linear fit (dashed)"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top") +
    guides(colour = guide_legend(override.aes = list(linetype = 0, size = 3)))
}

# ── Set 2 helper ──────────────────────────────────────────────────────────────
make_cgsi_index_plot <- function(data, y_index, color_by) {

  plot_data <- data |>
    dplyr::distinct(country_code, country_name, cgsi,
                    region, income_group, .data[[y_index]]) |>
    dplyr::filter(
      !is.na(cgsi),
      !is.na(.data[[y_index]]),
      !is.na(.data[[color_by]])
    )

  ggplot(plot_data, aes(x = cgsi, y = .data[[y_index]])) +
    geom_point(aes(color = .data[[color_by]]), alpha = 0.7, size = 2) +
    ggrepel::geom_text_repel(
      aes(label = country_code, color = .data[[color_by]]),
      size = 2.5, max.overlaps = 20, show.legend = FALSE
    ) +
    geom_smooth(
      method = "lm", formula = y ~ x,
      se = FALSE, color = "grey30", linetype = "dashed", linewidth = 0.9,
      na.rm     = TRUE
    ) +
    scale_color_manual(
      values = if (color_by == "region") {
        scales::hue_pal()(length(unique(plot_data[[color_by]])))
      } else {
        c("#00274C", "#2F65A7", "#00B2A9", "#702082")
      },
      name = tools::toTitleCase(gsub("_", " ", color_by))
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      title    = glue::glue("CGSI vs {toupper(y_index)}"),
      subtitle = glue::glue("Color = {gsub('_', ' ', color_by)}"),
      x        = "CGSI",
      y        = toupper(y_index),
      caption  = "Overall linear fit (dashed)"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top") +
    guides(colour = guide_legend(override.aes = list(linetype = 0, size = 3)))
}

# ── Batch save: Set 1 (cgsi vs outcomes) ──────────────────────────────────────
cgsi_outcomes     <- unique(cgsi_base$outcome)
cgsi_color_vers   <- c("region", "income_group")
output_dir_cgsi   <- "analysis/figs/corr/cgsi"
dir.create(output_dir_cgsi, recursive = TRUE, showWarnings = FALSE)

purrr::pwalk(
  tidyr::expand_grid(outcome_var = cgsi_outcomes, color_by = cgsi_color_vers),
  function(outcome_var, color_by) {
    p <- make_cgsi_outcome_plot(cgsi_base, outcome_var, color_by)
    ggplot2::ggsave(
      filename = file.path(output_dir_cgsi,
        glue::glue("cgsi_{snakecase::to_snake_case(outcome_var)}_{color_by}.png")),
      plot = p, width = 12, height = 8, dpi = 300, bg = "white"
    )
  }
)

# ── Batch save: Set 2 (cgsi vs other indices) ─────────────────────────────────
other_indices <- c("gtmi", "dcei", "gtei", "psdi")

purrr::pwalk(
  tidyr::expand_grid(y_index = other_indices, color_by = cgsi_color_vers),
  function(y_index, color_by) {
    p <- make_cgsi_index_plot(corr_data_long, y_index, color_by)
    ggplot2::ggsave(
      filename = file.path(output_dir_cgsi,
        glue::glue("cgsi_vs_{y_index}_{color_by}.png")),
      plot = p, width = 12, height = 8, dpi = 300, bg = "white"
    )
  }
)







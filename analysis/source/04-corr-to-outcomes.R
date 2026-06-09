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


# data-load ----------------------------------------------------------------

devtools::load_all()

# Composite index scores — 2025 wave only
# Named gtmi_2025 (not groups_data) to avoid collision with gtmi_indicators
# objects that may exist in a dirty session environment.
gtmi_2025 <- gtmi_data |>         # Lazy data: raw-data/0.gtmi_data_cleaning.R
  filter(year == 2025)

# IMS adoption (2025 only, binary per-system indicators)
adoption <- ims_adoption

# Outcomes
library(cliaretl)

vdem_data   <- cliaretl::vdem_data
# vdem_core_v2juaccnt    — Judicial accountability
# vdem_core_v2x_execorr  — Executive corruption index (reversed & rescaled)
# vdem_core_v2x_pubcorr  — Public sector corruption (reversed & rescaled)

loggdp_data <- cliaretl::closeness_to_frontier_static
# log_gdp — Natural log of GDP per capita (PPP)

# Country classifications
country_class <- cliaretl::wb_income_and_region |>
  select(country_code, region, income_group)

# outcomes-clean ---------------------------------------------------------

vdem_clean <- vdem_data |>
  filter(year == 2024) |>
  select(
    country_code,
    vdem_core_v2juaccnt,         # Judicial accountability
    # vdem_core_v2stcritrecadm,  # State capacity – critical resources in administration
    vdem_core_v2x_execorr,       # Executive corruption index (reversed & rescaled)
    vdem_core_v2x_pubcorr        # Public sector corruption (reversed & rescaled)
  )

loggdp_clean <- loggdp_data |>
  select(
    country_code,
    # wdi_nygdppcapppkd,  # GDP per capita, PPP (constant 2017 international $)
    log_gdp              # Natural log of GDP per capita (PPP)
  )

# region-recode helper ---------------------------------------------------
recode_region <- function(x) {
  dplyr::case_when(
    x == "East Asia & Pacific"                                ~ "EAP",
    x == "Europe & Central Asia"                             ~ "ECA",
    x == "Latin America & Caribbean"                         ~ "LAC",
    x == "Middle East, North Africa, Afghanistan & Pakistan" ~ "MENAAP",
    x == "South Asia"                                        ~ "SAR",
    x == "Sub-Saharan Africa"                                ~ "SSA",
    x == "North America"                                     ~ "NAM",
    TRUE ~ x
  )
}
# corr-data (composite indices × outcomes) --------------------------------
# Built directly from gtmi_2025 — no IMS join here.
# adoption_gtmi below is a separate path used only by corr_label.

corr_data <- gtmi_2025 |>
  left_join(country_class, by = "country_code") |>
  left_join(vdem_clean,    by = "country_code") |>
  left_join(loggdp_clean,  by = "country_code") |>
  filter(!is.na(region))

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
    region = recode_region(region)
  )

# adoption-annotated path (for corr_label / group_lines plots) -----------
adoption_transf <- adoption |>
  select(country_code, year, FMIS, TMIS, CMIS, EPMIS, DMIS, PIMIS) |>
  pivot_longer(
    cols = c(FMIS, TMIS, CMIS, EPMIS, DMIS, PIMIS),
    names_to = "ims_type",
    values_to = "adoption_status"
  ) |>
  select(-year)

adoption_gtmi <- adoption_transf |>
  left_join(gtmi_2025,     by = "country_code") |>
  left_join(country_class, by = "country_code") |>
  left_join(vdem_clean,    by = "country_code") |>
  left_join(loggdp_clean,  by = "country_code") |>
  filter(!is.na(region)) |>
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
    region = recode_region(region)
  )


corr_label <- adoption_gtmi |>
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

# data-quality -----------------------------------------------------------

outliers <- corr_label |>
  dplyr::filter(adoption_status %in% c("-", "0", "4.5")) |>
  dplyr::distinct(country_name, adoption_status) |>
  dplyr::arrange(adoption_status, country_name)



# batch-plots -------------------------------------------------------------
# One directory per color_by grouping; all index × outcome combos saved inside

indices        <- c("gtmi", "cgsi", "psdi", "dcei", "gtei")
outcomes       <- unique(corr_data_long$outcome)
color_versions <- c("region", "income_group", "grp")

# corr_data_long is already 1 row per country × outcome (no IMS duplication)
corr_base <- corr_data_long

purrr::walk(color_versions, function(col_by) {

  out_dir <- file.path("analysis/figs/corr", col_by)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  combos <- tidyr::expand_grid(x = indices, outcome_var = outcomes)

  purrr::pwalk(combos, function(x, outcome_var) {

    plot_data <- corr_base |> dplyr::filter(outcome == outcome_var)

    make_corr_scatter(
      data        = plot_data,
      x           = x,
      y           = "outcome_value",
      color_by    = col_by,
      group_lines = NULL,     # single overall line; swap to "adoption_label" if needed
      show_labels = TRUE,
      y_label     = outcome_var,
      filename    = file.path(out_dir,
        glue::glue("{x}_{snakecase::to_snake_case(outcome_var)}.png"))
    )
  })
})








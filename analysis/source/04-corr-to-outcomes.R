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
groups_data <- gtmi_data |> # Lazy data cleaned in raw-data/0.gtmi_data_cleaning.R
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

# data-quality -----------------------------------------------------------

outliers <- corr_data_long |>
  dplyr::filter(adoption_status %in% c("-", "0", "4.5")) |>
  dplyr::distinct(country_name, adoption_status) |>
  dplyr::arrange(adoption_status, country_name)



# batch-plots -------------------------------------------------------------
# One directory per color_by grouping; all index × outcome combos saved inside

indices       <- c("gtmi", "cgsi", "psdi", "dcei", "gtei")
outcomes      <- unique(corr_data_long$outcome)
color_versions <- c("region", "income_group", "grp")

# Deduplicate: one row per country × outcome (removes ims_type duplication)
corr_base <- corr_data_long |>
  dplyr::distinct(country_code, country_name, region, income_group, grp,
                  gtmi, cgsi, psdi, dcei, gtei, outcome, outcome_value)

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








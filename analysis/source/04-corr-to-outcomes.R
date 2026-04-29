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
    # vdem_core_v2stcritrecadm,    # State capacity – critical resources in administration
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
    )
  )


# plot -------------------------------------------------------------------

# Single plot
p <- corr_data_long |> 
  ggplot_corr_outcomes(x = "gtmi", group = "ims_type")

# All combinations, saved to disk
all_plots <- batch_corr_plots(
  corr_data_long,
  output_dir = "analysis/figs/corr"
)







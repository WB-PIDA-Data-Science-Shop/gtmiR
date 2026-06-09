# Correlation plots, region and income group
# Grouping indicators into 3 groups:
#  


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
  width = 10,
  height = 12
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

# pooling -------------------------------------------------------------------
# Interoperability (I1-4):          0–2 each → normalize to 0–1 → mean
# Public Sector Innovation (I46-48): I46/I47 are 0–2, I48 is 0–1 → rescale I48 ×2 → normalize → mean

indicators_2025 <- raw_panel |>
  filter(year == 2025) |>
  select(country_code,
         starts_with("wb_gtmi_i_")
        )

pooled <- indicators_2025 |>
  mutate(
    # Normalize each indicator to 0–1 before pooling
    # Interoperability: I-1 to I-4 (max = 2)
    across(c(wb_gtmi_i_1, wb_gtmi_i_2, wb_gtmi_i_3, wb_gtmi_i_4),
           ~ . / 2,
           .names = "{.col}_norm"),

    # Public Sector Innovation: I-46, I-47 (max = 2), I-48 (max = 1 → rescale ×2 then /2)
    wb_gtmi_i_46_norm = wb_gtmi_i_46 / 2,
    wb_gtmi_i_47_norm = wb_gtmi_i_47 / 2,
    wb_gtmi_i_48_norm = (wb_gtmi_i_48 * 2) / 2,  # rescale to 0–2 then normalize

    # Group scores: mean of available normalized indicators
    interoperability = rowMeans(
      pick(wb_gtmi_i_1_norm, wb_gtmi_i_2_norm,
           wb_gtmi_i_3_norm, wb_gtmi_i_4_norm),
      na.rm = TRUE
    ),
    public_sector_innovation = rowMeans(
      pick(wb_gtmi_i_46_norm, wb_gtmi_i_47_norm, wb_gtmi_i_48_norm),
      na.rm = TRUE
    )
  ) |>
  select(country_code, interoperability, public_sector_innovation)

# outcomes-clean ---------------------------------------------------------

country_class <- cliaretl::wb_income_and_region |>
  select(country_code, region, income_group)

vdem_clean <- vdem_data |>
  filter(year == 2024) |>
  select(
    country_code,
    vdem_core_v2juaccnt,
    vdem_core_v2x_execorr,
    vdem_core_v2x_pubcorr
  )

loggdp_clean <- loggdp_data |>
  select(country_code, log_gdp)

# corr-data --------------------------------------------------------------

# grp (maturity band A–D) comes from gtmi_data (composite index panel), 2025 wave

grp_lookup <- gtmi_data |> 
    filter(year == 2025) |>
    select(country_code, grp)

corr_data <- pooled |>
  left_join(grp_lookup,    by = "country_code") |>
  left_join(country_class, by = "country_code") |>
  left_join(vdem_clean,    by = "country_code") |>
  left_join(loggdp_clean,  by = "country_code") |>
  filter(!is.na(region)) |>
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
  ) |> 
  filter(!is.na(grp)) |> 
  filter(!is.na(income_group)) 

corr_data_long <- corr_data |>
  pivot_longer(
    cols      = c(vdem_core_v2juaccnt, vdem_core_v2x_execorr,
                  vdem_core_v2x_pubcorr, log_gdp),
    names_to  = "outcome",
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

# batch-plots -------------------------------------------------------------

indices        <- c("interoperability", "public_sector_innovation")
outcomes       <- unique(corr_data_long$outcome)
color_versions <- c("region", "income_group", "grp")

purrr::walk(color_versions, function(col_by) {

  out_dir <- file.path("analysis/figs/indicators_corr", col_by)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  combos <- tidyr::expand_grid(x = indices, outcome_var = outcomes)

  purrr::pwalk(combos, function(x, outcome_var) {

    plot_data <- corr_data_long |> dplyr::filter(outcome == outcome_var)

    make_corr_scatter(
      data        = plot_data,
      x           = x,
      y           = "outcome_value",
      color_by    = col_by,
      group_lines = NULL,
      show_labels = TRUE,
      y_label     = outcome_var,
      x_label     = snakecase::to_title_case(x),
      filename    = file.path(out_dir,
        glue::glue("{x}_{snakecase::to_snake_case(outcome_var)}.png"))
    )
  })
})

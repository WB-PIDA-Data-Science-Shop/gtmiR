# Trends analysis draft saved in the "figs/diffs"
# This script is a draft for the trends analysis. 
# It will be used to explore evolution of the GTMI groups and to create the figures for the trends analysis.

# set-up -----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(purrr)
library(readxl)


theme_set(
  theme_light() +
    theme(
      text = element_text(size = 16, family = "Segoe UI Semibold"),
      axis.text.x = element_text(size = 12, hjust = .5),
      axis.text.y = element_text(size = 12),
      plot.title = element_text(size = 22, face = "bold"),
      plot.subtitle = element_text(size = 16),
      plot.background = element_blank(),
      plot.caption = element_text(hjust = 0, size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.position = "none"
    )
)

ggsave <- partial(
  ggplot2::ggsave,
  bg = "white",
  width = 14,
  height = 8
)


# load-data --------------------------------------------------------------

devtools::load_all()

groups_data <- gtmi_data # Lazy data cleaned in raw-data/0.gtmi_data_cleaning.R

writexl::write_xlsx(
  groups_data,
   here::here("raw-data",
    "output", 
    "groups_data.xlsx"))

# Use latest country groups class for change funs
# remotes::install_github("WB-PIDA-Data-Science-Shop/pigoar2026")
# remotes::install_github("WB-PIDA-Data-Science-Shop/cliaretl")

country_class <- cliaretl::wb_income_and_region


# analysis ----------------------------------------------------------------
# Explore the evolution of the GTMI groups over time using change in group viz

# Compute differences for all indicators (2022 → 2025 only)
indicators <- c("gtmi", "cgsi", "psdi", "dcei", "gtei")

gtmi_diffs <- purrr::map_dfr(indicators, function(ind) {
  compute_gtmi_diff(groups_data, ind, 2022, 2025)
})

# left join with country classification to get income group and region for plotting
gtmi_diffs <- gtmi_diffs |>
  left_join(country_class |> select(country_code, income_group, region), by = "country_code") |>
  select(-grp)

#Prepare facet groups:
gtmi_classified <- gtmi_diffs |>
  classify_gtmi_group(2022) |>
  classify_gtmi_group(2025)

# All indicators at once — returns a named list of plots
plots <- plot_gtmi_time_trends(
  gtmi_classified,
  indicator   = c("gtmi", "cgsi", "psdi", "dcei", "gtei"),
  grouping    = "income_group",
  group_order = c("High income", "Upper middle income",
                  "Lower middle income", "Low income")
)

# Save all plots
output_dir <- here::here("analysis", "figs", "diffs")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
purrr::iwalk(plots, ~ggsave(file.path(output_dir, paste0(.y, "_income.png")), .x))










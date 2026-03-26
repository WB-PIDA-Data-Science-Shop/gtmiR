# Trends analysis draft
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

groups_data <- gtmi2025 # Lazy data cleaned in raw-data/0.gtmi_data_cleaning.R

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

# Compute differences for all indicators across both periods
indicators <- c("gtmi", "cgsi", "psdi", "dcei", "gtei")

periods    <- list(c(2020, 2022), c(2022, 2025), c(2020, 2025))

gtmi_diffs <- purrr::map_dfr(indicators, function(ind) {
  purrr::map_dfr(periods, function(p) {
    compute_gtmi_diff(groups_data, ind, p[1], p[2])
  })
})

# left join with country classification to get income group and region for plotting
gtmi_diffs <- gtmi_diffs |>
  left_join(country_class |> select(country_code, income_group, region), by = "country_code")


# Create plots and save to output directory for all indicator/period/grouping combinations

plot_specs <- tidyr::expand_grid(
  indicator   = c("gtmi", "cgsi", "psdi", "dcei", "gtei"),
  from_year   = c(2020, 2022),
  to_year     = c(2022, 2025),
  grouping    = c("grp", "income_group", "region")
) |>
  filter(from_year < to_year)

output_dir <- here::here("analysis", "figs")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

purrr::pwalk(plot_specs, function(indicator, from_year, to_year, grouping) {
  p <- gtmi_diffs |>
    filter(.data$indicator == .env$indicator,
           .data$from_year == .env$from_year,
           .data$to_year   == .env$to_year) |>
    generate_gtmi_diff_plot(
      grouping = grouping,
      title    = glue::glue("Change in {toupper(indicator)} ({from_year} \u2192 {to_year}) by {grouping}")
    )

  filename <- glue::glue("{output_dir}/{indicator}_{from_year}_{to_year}_{grouping}.png")
  ggsave(filename, plot = p)
})

# Dumbells for GTMI change by country, facetted by grouping tier




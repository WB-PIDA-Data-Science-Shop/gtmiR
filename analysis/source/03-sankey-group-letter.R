# Sankey events by group letter
# This script is a draft for the sanky trends on classification

# set-up -----------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(stringr)
library(readr)
library(purrr)
library(readxl)
library(ggsankey)
 



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



# sewt-up ----------------------------------------------------------------

devtools::load_all()

groups_data <- gtmi2025 # Lazy data cleaned in raw-data/0.gtmi_data_cleaning.R

writexl::write_xlsx(
  groups_data,
   here::here("raw-data",
    "output", 
    "groups_data.xlsx"))

country_class <- cliaretl::wb_income_and_region


# analysis ----------------------------------------------------------------

#Prepare facet groups:
gtmi_classified <- groups_data |>
  classify_gtmi_group(2020) |>
  classify_gtmi_group(2022) |>
  classify_gtmi_group(2025)

gtmi_label <- gtmi_classified |>
  # Coalesce the 3 year-specific group cols into one per indicator
  mutate(
    gtmi_group = coalesce(gtmi_group_2020, gtmi_group_2022, gtmi_group_2025),
    cgsi_group = coalesce(cgsi_group_2020, cgsi_group_2022, cgsi_group_2025),
    psdi_group = coalesce(psdi_group_2020, psdi_group_2022, psdi_group_2025),
    dcei_group = coalesce(dcei_group_2020, dcei_group_2022, dcei_group_2025),
    gtei_group = coalesce(gtei_group_2020, gtei_group_2022, gtei_group_2025)
  ) |>
  select(year, country_code, country_name, grp,
         gtmi, cgsi, psdi, dcei, gtei,
         gtmi_group, cgsi_group, psdi_group, dcei_group, gtei_group) |>
  # Pivot indicator values to long
  pivot_longer(
    cols      = c(gtmi, cgsi, psdi, dcei, gtei),
    names_to  = "indicator",
    values_to = "value"
  ) |>
  # Match each indicator to its group column
  mutate(
    group = case_when(
      indicator == "gtmi" ~ gtmi_group,
      indicator == "cgsi" ~ cgsi_group,
      indicator == "psdi" ~ psdi_group,
      indicator == "dcei" ~ dcei_group,
      indicator == "gtei" ~ gtei_group
    )
  ) |>
  select(year, country_code, country_name, grp, indicator, value, group)


# Create indicator lookup table
indicator_labels <- tribble(
  ~indicator, ~indicator_name,
  "gtmi", "GovTech Maturity Index (GTMI)",
  "cgsi", "Citizen Engagement & Service Index (CGSI)",
  "psdi", "Public Services Delivery Index (PSDI)",
  "dcei", "Data & Cybersecurity Index (DCEI)",
  "gtei", "GovTech Enabler Index (GTEI)"
)

# Add indicator names to gtmi_label
gtmi_long <- gtmi_label |>
  left_join(indicator_labels, by = "indicator")


# transform data ---------------------------------------------------------
# Pivot to wide (one row per country × indicator, group per year as columns)
# then use make_long to create the Sankey format

sankey_by_indicator <- gtmi_long |>
  select(country_code, indicator, indicator_name, year, group) |>
  pivot_wider(
    id_cols    = c(country_code, indicator, indicator_name),
    names_from = year,
    names_prefix = "y",
    values_from = group
  ) |>
  drop_na(y2020, y2022, y2025) |>
  group_split(indicator)

# Name the list elements by indicator
names(sankey_by_indicator) <- map_chr(sankey_by_indicator, ~unique(.x$indicator))

# Transform each indicator's data into Sankey format
sankey_by_indicator <- imap(sankey_by_indicator, function(df, ind) {
  ind_name <- unique(df$indicator_name)
  df |>
    ggsankey::make_long(y2020, y2022, y2025) |>
    mutate(
      indicator_name = ind_name,
      x      = recode(x,      "y2020" = "2020", "y2022" = "2022", "y2025" = "2025"),
      next_x = recode(next_x, "y2020" = "2020", "y2022" = "2022", "y2025" = "2025")
    )
})


# Sankey plots ------------------------------------------------------------

group_colors <- c("A" = "#4DAF4A", "B" = "#377EB8", "C" = "#FF7F00", "D" = "#E41A1C")

sankey_plots <- imap(sankey_by_indicator, function(df, ind) {

  # Count countries per node per year and compute percentage
  n_countries <- df |>
    filter(!is.na(node)) |>
    count(x, node, name = "n") |>
    group_by(x) |>
    mutate(pct = round(n / sum(n) * 100)) |>
    ungroup() |>
    mutate(node_label = paste0(node, "\n", n, " (", pct, "%)"))

  df |>
    filter(!is.na(node)) |>
    left_join(n_countries, by = c("x", "node")) |>
    mutate(
      node      = factor(node,      levels = c("D", "C", "B", "A")),
      next_node = factor(next_node, levels = c("D", "C", "B", "A"))
    ) |>
    ggplot(aes(x = x, next_x = next_x,
               node = node, next_node = next_node,
               fill = factor(node, levels = c("D", "C", "B", "A")),
               label = node_label)) +
    ggsankey::geom_sankey(flow.alpha = 0.4, node.color = "white") +
    ggsankey::geom_sankey_label(size = 3, fill = "white") +
    scale_fill_manual(
      values = group_colors,
      labels = c("A" = "A: Extensive", "B" = "B: Significant",
                 "C" = "C: Medium",    "D" = "D: Low"),
      drop = FALSE
    ) +
    labs(
      title    = unique(df$indicator_name),
      subtitle = "Country group transitions (2020 \u2192 2022 \u2192 2025)",
      x        = "Year",
      fill     = "Group"
    ) +
    theme(legend.position = "bottom",
          axis.title.y = element_blank(),
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 4, hjust = 0.5, angle = 45),
    )
})

# Save all sankey plots
output_dir <- here::here("analysis", "figs", "sankey")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

iwalk(sankey_plots, ~ ggsave(
  file.path(output_dir, paste0(.y, "_sankey_flow.png")), .x
))



# table of proportions ---------------------------------------------------




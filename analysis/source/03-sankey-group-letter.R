# Sankey events by group letter saved in "figs/sankey"
# This script is a draft for the sanky trends on classification

# set-up -----------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(stringr)
library(readr)
library(purrr)
library(readxl)
library(ggsankey)
library(dplyr)
 



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

groups_data <- gtmi_data # Lazy data cleaned in raw-data/0.gtmi_data_cleaning.R

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




# outperform countries ---------------------------------------------------
# Countries that made "leap" upgrades in GTMI group: D→B or C→A (2022→2025)

leap_upgrades <- gtmi_long |>
  filter(indicator == "gtmi", year %in% c(2022, 2025)) |>
  select(country_code, country_name, year, group) |>
  pivot_wider(names_from = year, values_from = group, names_prefix = "grp_") |>
  filter(
    (grp_2022 == "D" & grp_2025 == "B") |
    (grp_2022 == "C" & grp_2025 == "A")
  ) |>
  mutate(
    transition = paste0(grp_2022, " \u2192 ", grp_2025)
  ) |>
  left_join(
    country_class |> select(country_code, income_group, region),
    by = "country_code"
  ) |>
  left_join(
    groups_data |>
      filter(year %in% c(2022, 2025)) |>
      select(country_code, year, gtmi) |>
      pivot_wider(names_from = year, values_from = gtmi, names_prefix = "gtmi_"),
    by = "country_code"
  ) |>
  mutate(gtmi_change = round(gtmi_2025 - gtmi_2022, 3)) |>
  arrange(transition, desc(gtmi_change))

leap_upgrades


# 2025-levels-shares -----------------------------------------------------
# Stacked bar: for each GTMI group (A–D), what share of countries comes from
# each income level? Faceted by year (2022 vs 2025).

income_colors <- c(
  "High income"          = "#440154",  # deep purple
  "Upper middle income"  = "#31688E",  # steel blue
  "Lower middle income"  = "#35B779",  # mid green
  "Low income"           = "#009FDA"   # cyan
)

income_levels <- c(
  "High income",
  "Upper middle income",
  "Lower middle income",
  "Low income"
)

shares_data <- gtmi_long |>
  filter(indicator == "gtmi", year %in% c(2022, 2025), !is.na(group)) |>
  select(country_code, year, group) |>
  distinct() |>
  left_join(
    country_class |> select(country_code, income_group),
    by = "country_code"
  ) |>
  filter(!is.na(income_group)) |>
  mutate(
    group        = factor(group, levels = c("A", "B", "C", "D")),
    income_group = factor(income_group, levels = income_levels),
    year         = factor(year)
  ) |>
  count(year, group, income_group, name = "n") |>
  group_by(year, group) |>
  mutate(
    total = sum(n),
    pct   = round(n / total * 100)
  ) |>
  ungroup() |>
  mutate(bar_label = paste0(n, "\n(", pct, "%)"))

p_shares <- ggplot(shares_data,
                   aes(x = group, y = pct, fill = income_group)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.3) +
  geom_text(
    aes(label = bar_label),
    position = position_stack(vjust = 0.5),
    size      = 3,
    color     = "white",
    lineheight = 0.9
  ) +
  scale_fill_manual(
    values = income_colors,
    breaks = income_levels,
    name   = "Income group"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    limits = c(0, 101),
    expand = c(0, 0)
  ) +
  facet_wrap(~year, nrow = 1) +
  labs(
    title    = "Income composition within GTMI groups (2022 vs 2025)",
    subtitle = "Share of countries per income level within each group (A = Extensive \u2192 D = Low)",
    x        = "GTMI Group",
    y        = "Share of countries (%)",
    caption  = "Source: World Bank GovTech Dataset. n shown inside bars."
  ) +
  theme(
    legend.position  = "bottom",
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2))

p_shares

ggplot2::ggsave(
  filename = here::here("analysis", "figs", "sankey", "income", "income_share_by_grp_year.png"),
  plot     = p_shares,
  width    = 12, height = 7, dpi = 300, bg = "white"
)


# treemap comparison -----------------------------------------------------
# Side-by-side treemaps (2022 vs 2025): tile size = n countries,
# hierarchy: GTMI group → income level, coloured by income group

library(treemap)

treemap_data <- shares_data |>
  select(year, group, income_group, n) |>
  mutate(
    group        = as.character(group),
    income_group = as.character(income_group),
    year         = as.character(year)
  )

# Colour index aligned to income_levels order (viridis-derived, matches income_colors)
treemap_palette <- c("#440154", "#31688E", "#35B779", "#009FDA")

out_dir_tree <- here::here("analysis", "figs", "sankey", "income")
dir.create(out_dir_tree, recursive = TRUE, showWarnings = FALSE)

for (yr in c("2022", "2025")) {

  png(
    filename = file.path(out_dir_tree, paste0("treemap_income_grp_", yr, ".png")),
    width    = 2400, height = 1600, res = 200, bg = "white"
  )

  treemap(
    dtf            = treemap_data |> dplyr::filter(year == yr),
    index          = c("group", "income_group"),   # hierarchy: group > income
    vSize          = "n",
    vColor         = "income_group",
    type           = "categorical",
    palette        = treemap_palette,
    title          = paste0("Income composition within GTMI groups \u2014 ", yr),
    title.legend   = "Income group",
    fontsize.title = 16,
    fontsize.labels = c(18, 12),          # group label, income label
    fontface.labels = c("bold", "plain"),
    align.labels   = list(c("center", "top"), c("center", "center")),
    border.col     = c("white", "white"),
    border.lwds    = c(3, 1),
    overlap.labels = 0.5,
    inflate.labels = FALSE,
    bg.labels      = 0                    # transparent label backgrounds
  )

  dev.off()
}

message("Treemaps saved to: ", out_dir_tree)



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

## Exclude NIC (Nicaragua) from sankey/summary computations so it is not
## present in the generated sankey figures or the group counts.
## This removes NIC early so all downstream derived objects exclude it.
groups_data_raw <- gtmi_data |>
  filter(country_code != "NIC") # Exclude Nicaragua (NIC)

writexl::write_xlsx(
  groups_data,
   here::here("raw-data",
    "output", 
    "groups_data.xlsx"))

country_class <- cliaretl::wb_income_and_region


# Group labels (defined early for reuse across all visualizations) ----

grp_labels <- c("A" = "A: Extensive", "B" = "B: Significant",
                "C" = "C: Medium",    "D" = "D: Low")


# analysis ----------------------------------------------------------------
groups_data <- groups_data_raw |>
  # clean all country code is correspondent to the country_class data anti_join]
  # Remove any countries diff grp to  A,B,C,D or NA
  filter(grp %in% c("A", "B", "C", "D"))

### Income group non classified country codes:
 non_class_cc <- groups_data_raw |>
  anti_join(country_class, by = c("country_code" = "country_code")) |>
  distinct(country_code, country_name, year, grp) |> 
  #merge with country_class to get income group and region
  left_join(country_class, by = c("country_code" = "country_code")) 
  

#Prepare facet groups:
gtmi_classified <- groups_data |>
  classify_gtmi_group(2020) |>
  classify_gtmi_group(2022) |>
  classify_gtmi_group(2025)

# When we pivot to long we must pick the year-specific group column so that
# counts for a given year use the classification for that same year (e.g.
# gtmi_group_2025 for year 2025). Previously the script coalesced across
# years which mixed classifications and produced mismatched counts.
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
  group_split(indicator, .keep = TRUE)

# Name the list elements by indicator
names(sankey_by_indicator) <- map_chr(sankey_by_indicator, ~unique(.x$indicator))

# Transform each indicator's data into Sankey format
sankey_by_indicator <- imap(sankey_by_indicator, function(df, ind) {
  ind_name <- unique(df$indicator_name)
  
  df |>
    ggsankey::make_long(y2020, y2022, y2025) |>
    mutate(
      indicator = ind,
      indicator_name = ind_name
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
      labels = grp_labels,
      drop = FALSE
    ) +
    labs(
      title    = unique(df$indicator_name),
      subtitle = "Country group transitions (2020 → 2022 → 2025)",
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
  file.path(output_dir, paste0(.y, "_sankey_flow_final.png")), .x
))



# Verification / export for reproducibility
# Write a CSV with 2025 GTMI group membership and a small counts table so you
# can cross-check the numbers in your requested table. This uses the
# year-specific group column (gtmi_group_2025) and excludes NIC (filtered
# earlier), matching the sankey input rules.
output_verify_dir <- here::here("analysis", "figs", "sankey", "income", "verify")
dir.create(output_verify_dir, recursive = TRUE, showWarnings = FALSE)

# 2025 membership
final_members_2025 <- gtmi_classified |>
  filter(year == 2025) |>
  mutate(group = gtmi_group_2025) |>
  select(country_code, country_name, group) |>
  distinct()

readr::write_csv(final_members_2025, file.path(output_verify_dir, "gtmi_group_members_2025.csv"))

final_counts_2025 <- final_members_2025 |>
  count(group, name = "n") |>
  arrange(desc(n))

readr::write_csv(final_counts_2025, file.path(output_verify_dir, "gtmi_group_counts_2025.csv"))

# Also create an "aligned" counts table using only countries present in all
# three years (this is what the sankey uses) so you can directly compare.
countries_all3 <- gtmi_label |>
  distinct(country_code, year) |>
  pivot_wider(names_from = year, values_from = year, names_prefix = "y") |>
  drop_na(y2020, y2022, y2025) |>
  pull(country_code)

aligned_members_2025 <- final_members_2025 |>
  filter(country_code %in% countries_all3)

readr::write_csv(aligned_members_2025, file.path(output_verify_dir, "gtmi_group_members_2025_aligned_with_sankey.csv"))

aligned_counts_2025 <- aligned_members_2025 |>
  count(group, name = "n") |>
  arrange(desc(n))

readr::write_csv(aligned_counts_2025, file.path(output_verify_dir, "gtmi_group_counts_2025_aligned_with_sankey.csv"))


## Reconciliation with authoritative GTMI group list (2025)
## The list below is taken from the authoritative table you provided. We
## normalize names for robust matching, then compare to the generated
## membership to report matches, missing and unexpected entries.
authoritative_2025 <- tribble(
  ~group, ~country_name,
  "A", "Albania",
  "A", "Argentina",
  "A", "Armenia",
  "A", "Australia",
  "A", "Austria",
  "A", "Azerbaijan",
  "A", "Bahrain",
  "A", "Bangladesh",
  "A", "Belarus",
  "A", "Belgium",
  "A", "Bhutan",
  "A", "Brazil",
  "A", "Bulgaria",
  "A", "Cabo Verde",
  "A", "Canada",
  "A", "Chile",
  "A", "Colombia",
  "A", "Costa Rica",
  "A", "Croatia",
  "A", "Cyprus",
  "A", "Czech Republic",
  "A", "Denmark",
  "A", "Dominican Republic",
  "A", "Ecuador",
  "A", "Egypt",
  "A", "Estonia",
  "A", "Finland",
  "A", "Germany",
  "A", "Greece",
  "A", "Hungary",
  "A", "Iceland",
  "A", "India",
  "A", "Indonesia",
  "A", "Ireland",
  "A", "Italy",
  "A", "Japan",
  "A", "Jordan",
  "A", "Kazakhstan",
  "A", "Kenya",
  "A", "Korea, Rep.",
  "A", "Kosovo◊",
  "A", "Latvia",
  "A", "Lithuania",
  "A", "Luxembourg",
  "A", "Malaysia",
  "A", "Malta",
  "A", "Mauritius",
  "A", "Mexico",
  "A", "Moldova",
  "A", "Mongolia",
  "A", "Netherlands",
  "A", "New Zealand",
  "A", "North Macedonia",
  "A", "Oman",
  "A", "Panama",
  "A", "Peru",
  "A", "Portugal",
  "A", "Qatar",
  "A", "Russian Federation",
  "A", "Rwanda",
  "A", "Saudi Arabia",
  "A", "Serbia",
  "A", "Singapore",
  "A", "Slovenia",
  "A", "Spain",
  "A", "Sweden",
  "A", "Switzerland",
  "A", "Tanzania",
  "A", "Thailand",
  "A", "Türkiye",
  "A", "Uganda",
  "A", "Ukraine◊",
  "A", "United Arab Emirates",
  "A", "United Kingdom",
  "A", "Uruguay",
  "A", "Uzbekistan",
  # starred / footnoted entries (kept but flagged)
  "A", "France*",
  "A", "Norway*",
  "A", "Taiwan, China*",
  "A", "United States of America*",

  "B", "Algeria",
  "B", "Angola",
  "B", "Bahamas",
  "B", "Benin",
  "B", "Bolivia",
  "B", "Cambodia",
  "B", "Côte d'Ivoire",
  "B", "El Salvador",
  "B", "Ethiopia◊",
  "B", "Fiji",
  "B", "Georgia",
  "B", "Ghana",
  "B", "Guatemala",
  "B", "Guinea",
  "B", "Honduras",
  "B", "Iran",
  "B", "Jamaica",
  "B", "Kuwait",
  "B", "Kyrgyz Republic",
  "B", "Liechtenstein",
  "B", "Nepal",
  "B", "Nigeria◊",
  "B", "Pakistan",
  "B", "Papua New Guinea◊",
  "B", "Paraguay",
  "B", "Philippines",
  "B", "Poland",
  "B", "Romania",
  "B", "South Africa",
  "B", "Sri Lanka",
  "B", "Togo",
  "B", "Tunisia",
  "B", "Vietnam",
  "B", "West Bank and Gaza◊",
  "B", "Zambia",
  # starred/alternate
  "B", "Brunei Darussalam*",
  "B", "China*",
  "B", "Hong Kong SAR, China*",
  "B", "Israel*",
  "B", "Montenegro*",
  "B", "Morocco*",
  "B", "Slovak Republic*",

  "C", "Barbados",
  "C", "Belize",
  "C", "Bosnia and Herzegovina",
  "C", "Botswana",
  "C", "Burkina Faso◊",
  "C", "Burundi◊",
  "C", "Cameroon◊",
  "C", "Congo, Dem. Rep.◊",
  "C", "Dominica",
  "C", "Eswatini",
  "C", "Gabon",
  "C", "Grenada",
  "C", "Iraq◊",
  "C", "Kiribati◊",
  "C", "Lao PDR",
  "C", "Lebanon◊",
  "C", "Lesotho",
  "C", "Liberia",
  "C", "Madagascar",
  "C", "Malawi",
  "C", "Maldives",
  "C", "Mauritania",
  "C", "Mozambique◊",
  "C", "Myanmar◊",
  "C", "Seychelles",
  "C", "Sierra Leone",
  "C", "St. Kitts and Nevis",
  "C", "St. Lucia",
  "C", "St. Vincent and the Grenadines",
  "C", "Tajikistan",
  "C", "Timor-Leste◊",
  "C", "Tonga",
  "C", "Trinidad and Tobago",
  "C", "Vanuatu",
  "C", "Zimbabwe◊",
  # starred/alternate
  "C", "Andorra*",
  "C", "Cuba*",
  "C", "Guyana*",
  "C", "Macao SAR, China*",
  "C", "Mali◊*",
  "C", "Monaco*",
  "C", "Namibia*",
  "C", "Samoa*",
  "C", "Senegal*",
  "C", "Solomon Islands◊*",
  "C", "Suriname*",
  "C", "Venezuela, RB◊*",

  "D", "Central African Republic◊",
  "D", "Comoros◊",
  "D", "Guinea-Bissau◊",
  "D", "Haiti◊",
  "D", "Marshall Islands◊",
  "D", "Micronesia, Fed. Sts.◊",
  "D", "Nauru",
  "D", "Niger◊",
  "D", "Palau",
  "D", "Somalia◊",
  "D", "Tuvalu◊",
  "D", "Yemen◊",
  # starred/alternate
  "D", "Afghanistan◊*",
  "D", "Antigua and Barbuda*",
  "D", "Chad◊*",
  "D", "Congo, Rep.◊*",
  "D", "Djibouti*",
  "D", "Equatorial Guinea*",
  "D", "Eritrea◊*",
  "D", "Gambia*",
  "D", "Korea, DPR*",
  "D", "Libya◊*",
  "D", "San Marino*",
  "D", "São Tomé and Principe◊*",
  "D", "South Sudan◊*",
  "D", "Sudan◊*",
  "D", "Syrian Arab Republic◊*",
  "D", "Turkmenistan*"
)

# Normalization helper: lowercase, remove punctuation/diacritics, collapse spaces
norm_name <- function(x) {
  x |> stringr::str_to_lower() |>
    stringr::str_replace_all("[[:punct:]◊*]", "") |> # remove punctuation and footnote marker
    stringr::str_replace_all("[[:space:]]+", " ") |> stringr::str_trim()
}

auth_df <- authoritative_2025 |>
  mutate(id_name = norm_name(country_name))

gen_df <- final_members_2025 |>
  mutate(id_name = norm_name(country_name))

# Matches: join on normalized name
matched <- gen_df |>
  left_join(auth_df, by = "id_name") |>
  select(country_code, country_name = country_name.x, group_generated = group.x, group_authoritative = group.y)

# Unexpected: generated names not found in authoritative list
unexpected <- matched |>
  filter(is.na(group_authoritative))

# Missing: authoritative names not found in generated list
missing <- auth_df |>
  anti_join(gen_df, by = "id_name") |>
  select(group, country_name)

recon_out <- list(
  matched = matched,
  unexpected = unexpected,
  missing = missing
)

readr::write_csv(matched, file.path(output_verify_dir, "recon_matched_2025.csv"))
readr::write_csv(unexpected, file.path(output_verify_dir, "recon_unexpected_generated_2025.csv"))
readr::write_csv(missing, file.path(output_verify_dir, "recon_missing_from_generated_2025.csv"))



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
    transition = paste0(grp_2022, " → ", grp_2025)
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


# DIGITAL DIVIDE – ABSOLUTE COUNTS (2020 vs 2025) -----------------------------

income_colors <- c(
  "Low income"          = "#009FDA",
  "Lower middle income" = "#35B779",
  "Upper middle income" = "#31688E",
  "High income"         = "#440154"
)

income_levels <- c(
  "High income",
  "Upper middle income",
  "Lower middle income",
  "Low income"
)

counts_data <- gtmi_long |>
  filter(
    indicator == "gtmi",
    year %in% c(2020, 2025),
    !is.na(group)
  ) |>
  select(country_code, year, group) |>
  distinct() |>
  left_join(
    country_class |>
      select(country_code, income_group),
    by = "country_code"
  ) |>
  filter(!is.na(income_group)) |>
  mutate(
    year = factor(year, levels = c(2020, 2025)),
    group = factor(group, levels = c("A", "B", "C", "D"))
  ) |>
  count(year, group, income_group, name = "n") |>
  group_by(year, group) |>
  mutate(total = sum(n)) |>
  ungroup()

totals <- counts_data |>
  distinct(year, group, total)

p_divide <- ggplot(
  counts_data,
  aes(
    x = group,
    y = n,
    fill = factor(income_group, levels = income_levels)
  )
) +
  geom_col(
    width = 0.65,
    color = "white",
    linewidth = 0.5
  ) +
  
  # Add count labels inside bars
  geom_text(
    aes(label = n),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 4
  ) +

  scale_fill_manual(
    values = income_colors,
    breaks = income_levels,
    name = "Income group"
  ) +

  scale_y_continuous(
    expand = expansion(mult = c(0, .05)),
    limits = c(0, 80)
  ) +

  facet_wrap(
    ~year,
    nrow = 1
  ) +

  scale_x_discrete(
    labels = grp_labels
  ) +

  labs(
    title = NULL,
    x = "GTMI Group",
    y = "Number of countries",
    fill = "Income group"
  ) +

  theme_minimal(base_size = 14) +

  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 11),

    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),

    strip.text = element_text(face = "bold", size = 12),

    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20)
  )



ggsave(
  here::here(
    "analysis",
    "figs",
    "sankey",
    "income",
    "digital_divide_widening_2020_2025_final.png"
  )
)




# TRANSITION ANALYSIS: 16 STACKED BARS (2020→2022 vs 2022→2025) -----------

# Prepare transition data for all group-year pairs
from_year <- 2020
to_year <- 2025

transition_data <- gtmi_long |>
  filter(
    indicator == "gtmi",
    year %in% c(2020, 2022, 2025),
    !is.na(group)
  ) |>
  select(country_code, year, group) |>
  pivot_wider(
    id_cols = country_code,
    names_from = year,
    values_from = group,
    names_prefix = "y"
  ) |>
  left_join(
    country_class |>
      select(country_code, income_group),
    by = "country_code"
  ) |>
  filter(!is.na(income_group)) |>
  mutate(
    income_group = factor(income_group, levels = income_levels)
  )

# Create two transitions: 2020→2022 and 2022→2025
transition_summary <- bind_rows(
  # 2020 → 2022 transitions
  transition_data |>
    drop_na(y2020, y2022) |>
    count(from_group = y2020, to_group = y2022, income_group) |>
    mutate(period = "2020→2022"),
  # 2022 → 2025 transitions
  transition_data |>
    drop_na(y2022, y2025) |>
    count(from_group = y2022, to_group = y2025, income_group) |>
    mutate(period = "2022→2025")
) |>
  mutate(
    from_group = factor(from_group, levels = c("A", "B", "C", "D")),
    to_group = factor(to_group, levels = c("A", "B", "C", "D")),
    period = factor(period, levels = c("2020→2022", "2022→2025"))
  )

# Create faceted plot: 4 starting groups × 2 periods = 8 facets
# Each facet shows where countries from that group moved to
p_transitions <- ggplot(
  transition_summary,
  aes(
    x = period,
    y = n,
    fill = factor(income_group, levels = income_levels)
  )
) +
  geom_col(
    width = 0.65,
    color = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(label = n),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.5
  ) +
  facet_grid(
    from_group ~ to_group,
    labeller = labeller(
      from_group = c(
        "A" = "Starting: A (Extensive)",
        "B" = "Starting: B (Significant)",
        "C" = "Starting: C (Medium)",
        "D" = "Starting: D (Low)"
      ),
      to_group = c(
        "A" = "→ Moved to A",
        "B" = "→ Moved to B",
        "C" = "→ Moved to C",
        "D" = "→ Moved to D"
      )
    )
  ) +
  scale_fill_manual(
    values = income_colors,
    breaks = income_levels,
    name = "Income group"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, .05))
  ) +
  labs(
    title = "GTMI Group Transitions by Income Level",
    subtitle = "Where countries moved from 2020→2022 and 2022→2025",
    x = "Period",
    y = "Number of Countries",
    fill = "Income group"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    strip.text = element_text(face = "bold", size = 10),
    axis.title.x = element_text(face = "bold", size = 11),
    axis.title.y = element_text(face = "bold", size = 11),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20)
  )

ggplot2::ggsave(
  filename = here::here("analysis", "figs", "sankey", "income", "transitions_matrix_16bars_final.png")
)



# ALTERNATIVE: Absolute counts per group per year, stacked by income ----



group_counts_by_year <- gtmi_long |>
  filter(
    indicator == "gtmi",
    year %in% c(2020, 2022, 2025),
    !is.na(group)
  ) |>
  select(country_code, year, group) |>
  distinct() |>
  left_join(
    country_class |>
      select(country_code, income_group),
    by = "country_code"
  ) |>
  filter(!is.na(income_group)) |>
  mutate(
    year = factor(year, levels = c(2020, 2022, 2025)),
    group = factor(group, levels = c("A", "B", "C", "D")),
    income_group = factor(income_group, levels = income_levels)
  ) |>
  count(year, group, income_group, name = "n")

p_transitions_linear <- ggplot(
  group_counts_by_year,
  aes(
    x = group,
    y = n,
    fill = income_group
  )
) +
  geom_col(
    width = 0.65,
    color = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(label = n),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.5
  ) +
  scale_fill_manual(
    values = income_colors,
    breaks = income_levels,
    name = "Income group"
  ) +
  scale_x_discrete(
    labels = grp_labels
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, .05))
  ) +
  facet_wrap(~year, nrow = 1) +
  labs(
    title = "GTMI Maturity Level by Income Group",
    subtitle = "Number of countries in each group, 2020 / 2022 / 2025",
    x = "GTMI Group",
    y = "Number of Countries",
    fill = "Income group"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    strip.text = element_text(face = "bold", size = 10),
    axis.title.x = element_text(face = "bold", size = 11),
    axis.title.y = element_text(face = "bold", size = 11),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20)
  )


ggsave(
  here::here(
    "analysis",
    "figs",
    "sankey",
    "income", "group_counts_by_year_2020_2022_2025_final.png")
)


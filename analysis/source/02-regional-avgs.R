# Global avgs by region saved in "analysis/figs/timet_avg&boxplot"
# This script is a draft for the average change by region analysis. 

# set-up -----------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(stringr)
library(readr)
library(purrr)
library(readxl)




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



# load-data --------------------------------------------------------------

devtools::load_all()

groups_data <- gtmi_data |> # Lazy data cleaned in raw-data/0.gtmi_data_cleaning.R
  select(-grp)

country_class <- cliaretl::wb_income_and_region

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



# region-data-transf ------------------------------------------------------------
# Compute regional averages for GTMI indicators in 2020, 2022 and 2025 and their differences

gtmi_panel <- groups_data |>
  left_join(country_class |> select(country_code, income_group, region), by = "country_code") |>
  mutate(region = recode_region(region)) |>
  filter(!is.na(region))  # exclude countries without region or income group classification


regional_avgs <- gtmi_panel |>
  filter(year %in% c(2020, 2022, 2025)) |>
  group_by(region, year) |>
  summarise(
    gtmi_avg = mean(gtmi, na.rm = TRUE),
    cgsi_avg = mean(cgsi, na.rm = TRUE),
    psdi_avg = mean(psdi, na.rm = TRUE),
    dcei_avg = mean(dcei, na.rm = TRUE),
    gtei_avg = mean(gtei, na.rm = TRUE)
  ) |>
  pivot_longer(cols = ends_with("_avg"), names_to = "indicator", values_to = "avg_value") |>
  group_by(region, indicator) |>
  arrange(year) |>
  ungroup() 

# Create indicator lookup table
indicator_labels <- tribble(
  ~indicator, ~indicator_name,
  "gtmi_avg", "GovTech Maturity Index (GTMI)",
  "cgsi_avg", "Core Government Systems Index (CGSI)",
  "psdi_avg", "Public Services Delivery Index (PSDI)",
  "dcei_avg", "Data & Cybersecurity Index (DCEI)",
  "gtei_avg", "GovTech Enabler Index (GTEI)"
)

# Add indicator names to regional_avgs
regional_avgs <- regional_avgs |>
  left_join(indicator_labels, by = "indicator")

regional_avgs_diff <- regional_avgs |>
  group_by(region, indicator) |>
  summarise(
    diff_2020_2022 = avg_value[year == 2022] - avg_value[year == 2020],
    diff_2022_2025 = avg_value[year == 2025] - avg_value[year == 2022],
    diff_2020_2025 = avg_value[year == 2025] - avg_value[year == 2020]
  ) |>
  pivot_longer(cols = starts_with("diff"), names_to = "period", values_to = "difference") |>
  ungroup()



# region avgs -------------------------------------------------------------

regional_avgs |>
  dplyr::mutate(
      region = stringr::str_wrap(region, width = 20)
    ) |> 
  ggplot(aes(x = year, y = avg_value, color = indicator_name, group = indicator_name)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3, shape = 15) +
  labs(
    x = "Year",
    y = "Regional GovTech Maturity Index Trends, 2020–2025",
    color = "GovTech Focus Areas",
    title = "Regional Indicator Averages Over Time",
    subtitle = "Average focus area values by region for 2020, 2022 and 2025",
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = scales::breaks_pretty()) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_grid(~region, scales = "free_y") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom",
    strip.text      = element_text(size = 10)
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave(
    here::here(
    "analysis", "figs", "timet_avg", "regional_avgs.png")
)


# income-data-transf ------------------------------------------------------------
# Compute regional averages for GTMI indicators in 2020, 2022 and 2025 and their differences

gtmi_panel <- groups_data |>
  left_join(country_class |> select(country_code, income_group, region), by = "country_code") |>
  mutate(region = recode_region(region)) |>
  filter(!is.na(income_group))

income_avgs <- gtmi_panel |>
  filter(year %in% c(2020, 2022, 2025)) |>
  group_by(income_group, year) |>
  summarise(
    gtmi_avg = mean(gtmi, na.rm = TRUE),
    cgsi_avg = mean(cgsi, na.rm = TRUE),
    psdi_avg = mean(psdi, na.rm = TRUE),
    dcei_avg = mean(dcei, na.rm = TRUE),
    gtei_avg = mean(gtei, na.rm = TRUE)
  ) |>
  pivot_longer(cols = ends_with("_avg"), names_to = "indicator", values_to = "avg_value") |>
  group_by(income_group, indicator) |>
  arrange(year) |>
  ungroup() 

 income_levels <- c(
    "High income",
    "Upper middle income",
    "Lower middle income",
    "Low income"
  )

# Add indicator names to income_groupal_avgs
income_groupal_avgs <- income_avgs |>
  left_join(indicator_labels, by = "indicator")

income_groupal_avgs_diff <- income_groupal_avgs |>
  group_by(income_group, indicator) |>
  summarise(
    diff_2020_2022 = avg_value[year == 2022] - avg_value[year == 2020],
    diff_2022_2025 = avg_value[year == 2025] - avg_value[year == 2022],
    diff_2020_2025 = avg_value[year == 2025] - avg_value[year == 2020]
  ) |>
  pivot_longer(cols = starts_with("diff"), names_to = "period", values_to = "difference") |>
  ungroup()



# income_group avgs -------------------------------------------------------------

income_groupal_avgs |>
  dplyr::mutate(
      income_group = factor(income_group, levels = income_levels)
    ) |> 
  ggplot(aes(x = year, y = avg_value, color = indicator_name, group = indicator_name)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3, shape = 15) +
  labs(
    x = "Year",
    y = "Regional GovTech Maturity Index Trends, 2020–2025",
    color = "GovTech Focus Areas",
    title = "Income Level Averages Over Time",
    subtitle = "Average focus area values by Income Group for 2020, 2022 and 2025",
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = scales::breaks_pretty()) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_grid(~income_group, scales = "free_y") +
  theme(
     axis.text.x = element_text(size = 4, hjust = 1, angle = 45),
    legend.position = "bottom",
    strip.text      = element_text(size = 10)
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave(
    here::here(
    "analysis", "figs", "timet_avg", "income_group_avgs.png")
)




# boxplot-analysis -------------------------------------------------------
# One plot per indicator: x = year, y = score, boxes neutral grey,
# jitter + diamonds coloured by grp, background bands show GTMI tier thresholds

grp_colors <- c(
  "A" = "#4DAF4A",
  "B" = "#377EB8",
  "C" = "#FF7F00",
  "D" = "#E41A1C"
)

# GTMI score threshold bands (y-axis background)
gtmi_bands <- tibble::tibble(
  ymin  = c(0.00, 0.25, 0.50, 0.75),
  ymax  = c(0.25, 0.50, 0.75, 1.00),
  grp   = c("D",  "C",  "B",  "A"),
  fill  = c("#E41A1C", "#FF7F00", "#377EB8", "#4DAF4A")
)

# Need grp on boxplot_data for jitter + diamond coloring
# grp_lookup: 2025 group assignment per country (one row per country_code)
grp_lookup <- gtmi_data |>
  filter(year == 2025) |>
  select(country_code, grp)

boxplot_data <- gtmi_panel |>
  left_join(grp_lookup, by = "country_code") |>
  filter(year %in% c(2020, 2022, 2025)) |>
  pivot_longer(
    cols      = c(gtmi, cgsi, psdi, dcei, gtei),
    names_to  = "indicator",
    values_to = "value"
  ) |>
  filter(!is.na(value), !is.na(region)) |>
  left_join(indicator_labels |>
              mutate(indicator = stringr::str_remove(indicator, "_avg")),
            by = "indicator") |>
  mutate(
    year = factor(year),
    grp  = factor(grp, levels = c("A", "B", "C", "D"))
  )

# output_dir_box <- "analysis/figs/boxplot"
# dir.create(output_dir_box, recursive = TRUE, showWarnings = FALSE)

# purrr::walk(unique(boxplot_data$indicator), function(ind) {

#   plot_data  <- boxplot_data |> filter(indicator == ind)
#   ind_label  <- unique(plot_data$indicator_name)

#   # Overall region mean reference line (all years pooled)
#   region_avg <- plot_data |>
#     dplyr::group_by(region) |>
#     dplyr::summarise(region_mean = mean(value, na.rm = TRUE), .groups = "drop")

#   # Per-year median per region — coloured by grp of the median country
#   year_region_median <- plot_data |>
#     dplyr::group_by(region, year) |>
#     dplyr::summarise(year_median = median(value, na.rm = TRUE), .groups = "drop") |>
#     dplyr::mutate(
#       grp = dplyr::case_when(
#         year_median >= 0.75 ~ "A",
#         year_median >= 0.50 ~ "B",
#         year_median >= 0.25 ~ "C",
#         TRUE                ~ "D"
#       ),
#       grp = factor(grp, levels = c("A", "B", "C", "D"))
#     )

#   p <- ggplot(plot_data, aes(x = year, y = value)) +
#     # Background tier bands
#     geom_rect(
#       data    = gtmi_bands,
#       inherit.aes = FALSE,
#       aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = fill),
#       alpha   = 0.25
#     ) +
#     scale_fill_identity() +
#     # Overall region mean reference line
#     geom_hline(
#       data      = region_avg,
#       aes(yintercept = region_mean),
#       color     = "grey30",
#       linetype  = "dashed",
#       linewidth = 0.4
#     ) +
#     # Neutral grey boxes (IQR + median bar)
#     geom_boxplot(
#       width         = 0.35,
#       outlier.shape = NA,
#       fill          = "grey85",
#       color         = "grey40",
#       alpha         = 0.6,
#       linewidth     = 0.5
#     ) +
#     # Country-level jitter — neutral, no grp color
#     geom_jitter(
#       color  = "grey50",
#       size   = 1.2,
#       alpha  = 0.45,
#       width  = 0.15
#     ) +
#     # Per-year median diamond coloured by grp
#     geom_point(
#       data  = year_region_median,
#       aes(x = year, y = year_median, color = grp),
#       shape  = 23,
#       size   = 4,
#       fill   = NA,
#       stroke = 1.8
#     ) +
#     scale_color_manual(values = grp_colors, name = "GTMI Group") +
#     scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
#     facet_wrap(~ region, nrow = 1) +
#     labs(
#       title    = glue::glue("{ind_label}"),
#       subtitle = "Distribution of country scores by year and region",
#       x        = "Year",
#       y        = "Score (0–1)",
#       caption  = paste0(
#         "Diamond outline = year median (coloured by GTMI group). ",
#         "Dashed line = overall region mean (all years pooled).\n",
#         "Background bands: A \u22650.75 (green), B 0.50\u20130.75 (blue), ",
#         "C 0.25\u20130.50 (orange), D <0.25 (red)."
#       )
#     ) +
#     theme(
#       legend.position  = "bottom",
#       strip.text       = element_text(size = 9),
#       panel.grid.minor = element_blank(),
#       panel.grid.major = element_blank(),
#       panel.border     = element_blank()
#     )

#   ggplot2::ggsave(
#     filename = file.path(output_dir_box, glue::glue("abcd_boxplot_{ind}_region.png")),
#     plot     = p,
#     width    = 14, height = 6, dpi = 300, bg = "white"
#   )
# })




# # boxplot-v3 (half-violin + thin boxplot, region AND income_group) --------
# # - geom_rect removed; tier bands replaced with y-axis breaks + subtle grid
# # - Dots AND diamonds coloured by grp; NA countries shown as grey
# # - notched boxplot dropped → plain slim boxplot avoids pinch artefacts
# # - Two output variants per indicator: region / income_group facets

# library(ggplot2)
# library(dplyr)
# library(glue)
# library(purrr)

# grp_colors <- c(
#   "A"  = "#4DAF4A",
#   "B"  = "#377EB8",
#   "C"  = "#FF7F00",
#   "D"  = "#E41A1C",
#   "NA" = "grey70"          # explicit bucket for unclassified countries
# )

# output_dir_v3 <- "analysis/figs/boxplot/v3"
# dir.create(output_dir_v3, recursive = TRUE, showWarnings = FALSE)

# # Helper: build median summary coloured by grp threshold
# build_median <- function(df, facet_var) {
#   df |>
#     group_by(across(all_of(c(facet_var, "year")))) |>
#     summarise(year_median = median(value, na.rm = TRUE), .groups = "drop") |>
#     mutate(
#       grp = case_when(
#         year_median >= 0.75 ~ "A",
#         year_median >= 0.50 ~ "B",
#         year_median >= 0.25 ~ "C",
#         TRUE                ~ "D"
#       ),
#       grp = factor(grp, levels = c("A", "B", "C", "D"))
#     )
# }

# # Helper: build mean reference line
# build_mean <- function(df, facet_var) {
#   df |>
#     group_by(across(all_of(facet_var))) |>
#     summarise(region_mean = mean(value, na.rm = TRUE), .groups = "drop")
# }

# # Shared plot builder ---------------------------------------------------------
# build_v3_plot <- function(plot_data, facet_var, facet_nrow = 1, ind_label) {

#   # Coerce NA grp to "NA" bucket → grey
#   plot_data <- plot_data |>
#     mutate(grp = factor(
#       if_else(is.na(as.character(grp)), "NA", as.character(grp)),
#       levels = c("A", "B", "C", "D", "NA")
#     ))

#   med_df  <- build_median(plot_data, facet_var)
#   mean_df <- build_mean(plot_data, facet_var)

#   facet_formula <- as.formula(paste("~", facet_var))

#   ggplot(plot_data, aes(x = year, y = value)) +

#     # ── Overall group mean reference line (all years pooled) ─────────────
#     geom_hline(
#       data      = mean_df,
#       aes(yintercept = region_mean),
#       color     = "grey65",
#       linetype  = "dashed",
#       linewidth = 0.35
#     ) +

#     # ── Slim IQR boxplot — structure only, no fill distraction ───────────
#     geom_boxplot(
#       width         = 0.3,
#       outlier.shape = NA,
#       fill          = NA,
#       color         = "grey60",
#       linewidth     = 0.4,
#       fatten        = 1.2      # slightly thinner median bar
#     ) +

#     # ── Country dots coloured by grp — main data layer ───────────────────
#     geom_jitter(
#       aes(color = grp),
#       width  = 0.09,
#       size   = 2,
#       alpha  = 0.6,
#       stroke = 0
#     ) +

#     # ── Year-median segment — same width as boxplot, coloured by grp ─────
#     geom_segment(
#       data = med_df |>
#         mutate(x_pos = as.numeric(factor(year, levels = c("2020", "2022", "2025")))),
#       aes(
#         x     = x_pos - 0.15,
#         xend  = x_pos + 0.15,
#         y     = year_median,
#         yend  = year_median,
#         color = grp
#       ),
#       linewidth = 2.0,
#       lineend   = "round"
#     ) +

#     # ── Scales ────────────────────────────────────────────────────────────
#     scale_color_manual(
#       values = grp_colors,
#       name   = "GTMI Group",
#       labels = c(
#         "A"  = "A  (\u22650.75)",
#         "B"  = "B  (0.50\u20130.75)",
#         "C"  = "C  (0.25\u20130.50)",
#         "D"  = "D  (<0.25)",
#         "NA" = "Unclassified"
#       ),
#       drop = FALSE
#     ) +
#     scale_y_continuous(
#       limits = c(0, 1),
#       breaks = c(0, 0.25, 0.50, 0.75, 1)
#     ) +

#     # ── Facet ─────────────────────────────────────────────────────────────
#     facet_wrap(facet_formula, nrow = facet_nrow, scales = "fixed") +

#     # ── Labels ────────────────────────────────────────────────────────────
#     labs(
#       title    = glue("{ind_label}"),
#       subtitle = "Country scores by year and region — 2020, 2022, 2025",
#       x        = NULL,
#       y        = "Score (0\u20131)",
#       caption  = paste0(
#         "Solid coloured line = year median (coloured by GTMI group threshold)  \u2502  ",
#         "Dots = individual countries  \u2502  ",
#         "Box = IQR  \u2502  ",
#         "Dashed grey line = group mean (all years pooled)"
#       )
#     ) +

#     # ── Theme: maximum whitespace ─────────────────────────────────────────
#     theme_minimal(base_size = 10) +
#     theme(
#       legend.position    = "bottom",
#       legend.key.size    = unit(0.4, "cm"),
#       legend.text        = element_text(size = 8),
#       strip.text         = element_text(size = 8.5, face = "bold"),
#       strip.background   = element_blank(),
#       axis.text.x        = element_text(size = 8),
#       panel.grid.minor   = element_blank(),
#       panel.grid.major.x = element_blank(),
#       panel.border       = element_blank(),
#       panel.spacing      = unit(0.5, "lines"),
#       plot.title         = element_text(face = "bold", size = 13),
#       plot.subtitle      = element_text(size = 9, color = "grey40",
#                                         margin = margin(b = 4)),
#       plot.caption       = element_text(size = 7.5, color = "grey50",
#                                         hjust = 0)
#     )
# }

# # ── Walk indicators, save both facet variants ───────────────────────────────
# purrr::walk(unique(boxplot_data$indicator), function(ind) {

#   plot_data <- boxplot_data |>
#     filter(indicator == ind)

#   # Ensure income_group is present (join if needed)
#   if (!"income_group" %in% names(plot_data)) {
#     plot_data <- plot_data |>
#       left_join(gtmi_panel |> distinct(country_code, income_group),
#                 by = "country_code")
#   }

#   ind_label <- unique(plot_data$indicator_name)

#   # — Variant A: facet by region ——————————————————————————————————————————
#   p_region <- build_v3_plot(plot_data, "region", facet_nrow = 1, ind_label)

#   ggplot2::ggsave(
#     file.path(output_dir_v3, glue("v3_{ind}_region.png")),
#     plot = p_region, width = 16, height = 6, dpi = 300, bg = "white"
#   )

#   # — Variant B: facet by income_group ————————————————————————————————————
#   p_income <- build_v3_plot(plot_data, "income_group", facet_nrow = 1, ind_label)

#   ggplot2::ggsave(
#     file.path(output_dir_v3, glue("v3_{ind}_income_group.png")),
#     plot = p_income, width = 12, height = 6, dpi = 300, bg = "white"
#   )
# })


# boxplot-v3 (half-violin + thin boxplot, region AND income_group) --------
# - geom_rect removed; tier bands replaced with y-axis breaks + subtle grid
# - Dots AND diamonds coloured by grp; NA countries shown as grey
# - notched boxplot dropped → plain slim boxplot avoids pinch artefacts
# - Two output variants per indicator: region / income_group facets
# - Facets ordered by descending median score (highest → lowest)

library(ggplot2)
library(dplyr)
library(glue)
library(purrr)

grp_colors <- c(
  "A"  = "#4DAF4A",
  "B"  = "#377EB8",
  "C"  = "#FF7F00",
  "D"  = "#E41A1C",
  "NA" = "grey70"
)

output_dir_v3 <- "analysis/figs/boxplot/v3"
dir.create(output_dir_v3, recursive = TRUE, showWarnings = FALSE)

build_median <- function(df, facet_var) {
  df |>
    group_by(across(all_of(c(facet_var, "year")))) |>
    summarise(year_median = median(value, na.rm = TRUE), .groups = "drop") |>
    mutate(
      grp = case_when(
        year_median >= 0.75 ~ "A",
        year_median >= 0.50 ~ "B",
        year_median >= 0.25 ~ "C",
        TRUE                ~ "D"
      ),
      grp = factor(grp, levels = c("A", "B", "C", "D"))
    )
}

build_mean <- function(df, facet_var) {
  df |>
    group_by(across(all_of(facet_var))) |>
    summarise(region_mean = mean(value, na.rm = TRUE), .groups = "drop")
}

# NEW: compute overall median per facet group (all years pooled) → used for ordering
build_facet_order <- function(df, facet_var) {
  df |>
    group_by(across(all_of(facet_var))) |>
    summarise(overall_median = median(value, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(overall_median)) |>
    pull(!!sym(facet_var))
}

build_v3_plot <- function(plot_data, facet_var, facet_nrow = 1, ind_label) {

  plot_data <- plot_data |>
    mutate(grp = factor(
      if_else(is.na(as.character(grp)), "NA", as.character(grp)),
      levels = c("A", "B", "C", "D", "NA")
    ))

  # ── Order facet variable by descending median ──────────────────────────
  facet_order <- build_facet_order(plot_data, facet_var)
  plot_data <- plot_data |>
    mutate(across(all_of(facet_var), \(x) factor(x, levels = facet_order)))

  med_df  <- build_median(plot_data, facet_var)
  mean_df <- build_mean(plot_data, facet_var)

  # Propagate the same factor ordering to summary frames so strips align
  med_df <- med_df |>
    mutate(across(all_of(facet_var), \(x) factor(x, levels = facet_order)))
  mean_df <- mean_df |>
    mutate(across(all_of(facet_var), \(x) factor(x, levels = facet_order)))

  facet_formula <- as.formula(paste("~", facet_var))

  ggplot(plot_data, aes(x = year, y = value)) +

    geom_hline(
      data      = mean_df,
      aes(yintercept = region_mean),
      color     = "grey65",
      linetype  = "dashed",
      linewidth = 0.35
    ) +

    geom_boxplot(
      width         = 0.3,
      outlier.shape = NA,
      fill          = NA,
      color         = "grey60",
      linewidth     = 0.4,
      fatten        = 1.2
    ) +

    geom_jitter(
      aes(color = grp),
      width  = 0.09,
      size   = 2,
      alpha  = 0.6,
      stroke = 0
    ) +

    geom_segment(
      data = med_df |>
        mutate(x_pos = as.numeric(factor(year, levels = c("2020", "2022", "2025")))),
      aes(
        x     = x_pos - 0.15,
        xend  = x_pos + 0.15,
        y     = year_median,
        yend  = year_median,
        color = grp
      ),
      linewidth = 2.0,
      lineend   = "round"
    ) +

    scale_color_manual(
      values = grp_colors,
      name   = "GTMI Group",
      labels = c(
        "A"  = "A  (\u22650.75)",
        "B"  = "B  (0.50\u20130.75)",
        "C"  = "C  (0.25\u20130.50)",
        "D"  = "D  (<0.25)",
        "NA" = "Unclassified"
      ),
      drop = FALSE
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.50, 0.75, 1)
    ) +

    facet_wrap(facet_formula, nrow = facet_nrow, scales = "fixed") +

    labs(
      title    = glue("{ind_label}"),
      subtitle = "Country scores by year and region — 2020, 2022, 2025",
      x        = NULL,
      y        = "Score (0\u20131)",
      caption  = paste0(
        "Solid coloured line = year median (coloured by GTMI group threshold)  \u2502  ",
        "Dots = individual countries  \u2502  ",
        "Box = IQR  \u2502  ",
        "Dashed grey line = group mean (all years pooled)"
      )
    ) +

    theme_minimal(base_size = 10) +
    theme(
      legend.position    = "bottom",
      legend.key.size    = unit(0.4, "cm"),
      legend.text        = element_text(size = 8),
      strip.text         = element_text(size = 8.5, face = "bold"),
      strip.background   = element_blank(),
      axis.text.x        = element_text(size = 8),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.border       = element_blank(),
      panel.spacing      = unit(0.5, "lines"),
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(size = 9, color = "grey40",
                                        margin = margin(b = 4)),
      plot.caption       = element_text(size = 7.5, color = "grey50",
                                        hjust = 0)
    )
}

purrr::walk(unique(boxplot_data$indicator), function(ind) {

  plot_data <- boxplot_data |>
    filter(indicator == ind)

  if (!"income_group" %in% names(plot_data)) {
    plot_data <- plot_data |>
      left_join(gtmi_panel |> distinct(country_code, income_group),
                by = "country_code")
  }

  ind_label <- unique(plot_data$indicator_name)

  p_region <- build_v3_plot(plot_data, "region", facet_nrow = 1, ind_label)

  ggplot2::ggsave(
    file.path(output_dir_v3, glue("v3_{ind}_region.png")),
    plot = p_region, width = 16, height = 6, dpi = 300, bg = "white"
  )

  p_income <- build_v3_plot(plot_data, "income_group", facet_nrow = 1, ind_label)

  ggplot2::ggsave(
    file.path(output_dir_v3, glue("v3_{ind}_income_group.png")),
    plot = p_income, width = 12, height = 6, dpi = 300, bg = "white"
  )
})


# Income group averages and boxplot analysis
# Mirrors 02-regional-avgs.R but groups by income level.

# set-up -----------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(stringr)
library(readr)
library(purrr)
library(readxl)
library(glue)
library(here)

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


# load-data --------------------------------------------------------------

devtools::load_all()

groups_data <- gtmi_data |>
  select(-grp)

country_class <- cliaretl::wb_income_and_region

income_levels <- c(
  "High income",
  "Upper middle income",
  "Lower middle income",
  "Low income"
)

# Fixed palette for income groups — light to dark, ordered high → low
income_colors <- c(
  "High income"          = "#1A6BB5",
  "Upper middle income"  = "#6BAED6",
  "Lower middle income"  = "#FDAE6B",
  "Low income"           = "#D94801"
)

# Group labels (defined early for reuse across all visualizations) ----

grp_labels <- c("A" = "A: Extensive", "B" = "B: Significant",
                "C" = "C: Medium",    "D" = "D: Low")

# grp lookup for boxplot (A–D bands)
grp_lookup <- gtmi_data |>
  filter(year == 2025) |>
  select(country_code, grp)

grp_colors <- c(
  "A" = "#4DAF4A",
  "B" = "#377EB8",
  "C" = "#FF7F00",
  "D" = "#E41A1C"
)

gtmi_bands <- tibble::tibble(
  ymin = c(0.00, 0.25, 0.50, 0.75),
  ymax = c(0.25, 0.50, 0.75, 1.00),
  grp  = c("D",  "C",  "B",  "A"),
  fill = c("#E41A1C", "#FF7F00", "#377EB8", "#4DAF4A")
)


# data-transf ------------------------------------------------------------

gtmi_panel <- groups_data |>
  left_join(country_class |> select(country_code, income_group, region), by = "country_code") |>
  filter(!is.na(income_group))

# Create indicator lookup table
indicator_labels <- tribble(
  ~indicator, ~indicator_name,
  "gtmi_avg", "GovTech Maturity Index (GTMI)",
  "cgsi_avg", "Core Government Systems Index (CGSI)",
  "psdi_avg", "Public Service Delivery Index (PSDI)",
  "dcei_avg", "Digital Citizen Engagement Index (DCEI)",
  "gtei_avg", "GovTech Enabling Index (GTEI)"
)


# income avgs (line chart) -----------------------------------------------

income_avgs <- gtmi_panel |>
  filter(year %in% c(2020, 2022, 2025)) |>
  group_by(income_group, year) |>
  summarise(
    gtmi_avg = mean(gtmi, na.rm = TRUE),
    cgsi_avg = mean(cgsi, na.rm = TRUE),
    psdi_avg = mean(psdi, na.rm = TRUE),
    dcei_avg = mean(dcei, na.rm = TRUE),
    gtei_avg = mean(gtei, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(cols = ends_with("_avg"), names_to = "indicator", values_to = "avg_value") |>
  left_join(indicator_labels, by = "indicator") |>
  mutate(income_group = factor(income_group, levels = income_levels))

income_avgs_diff <- income_avgs |>
  group_by(income_group, indicator) |>
  summarise(
    diff_2020_2022 = avg_value[year == 2022] - avg_value[year == 2020],
    diff_2022_2025 = avg_value[year == 2025] - avg_value[year == 2022],
    diff_2020_2025 = avg_value[year == 2025] - avg_value[year == 2020],
    .groups = "drop"
  ) |>
  pivot_longer(cols = starts_with("diff"), names_to = "period", values_to = "difference")

dir.create(here("analysis", "figs", "timet_avg"), recursive = TRUE, showWarnings = FALSE)

income_avgs |>
  ggplot(aes(x = year, y = avg_value, color = indicator_name, group = indicator_name)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3, shape = 15) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = scales::breaks_pretty()) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_grid(~ income_group) +
  labs(
    title    = "Income Group Indicator Averages Over Time",
    subtitle = "Average focus area values by income group for 2020, 2022 and 2025",
    x        = "Year",
    y        = "Score (0–1)",
    color    = "GovTech Focus Areas"
  ) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom",
    strip.text      = element_text(size = 10)
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave(here("analysis", "figs", "timet_avg", "income_avgs.png"))


# boxplot-analysis -------------------------------------------------------
# One plot per indicator: x = year, y = score, boxes neutral grey,
# jitter neutral grey, diamonds coloured by grp,
# facets = income group (ordered high → low), background bands = GTMI tiers

boxplot_data <- gtmi_panel |>
  left_join(grp_lookup, by = "country_code") |>
  filter(year %in% c(2020, 2022, 2025)) |>
  pivot_longer(
    cols      = c(gtmi, cgsi, psdi, dcei, gtei),
    names_to  = "indicator",
    values_to = "value"
  ) |>
  filter(!is.na(value), !is.na(income_group)) |>
  left_join(
    indicator_labels |> mutate(indicator = stringr::str_remove(indicator, "_avg")),
    by = "indicator"
  ) |>
  mutate(
    year         = factor(year),
    income_group = factor(income_group, levels = income_levels),
    grp          = factor(grp, levels = c("A", "B", "C", "D"), labels = grp_labels, exclude = NA)
  )

output_dir_box <- "analysis/figs/boxplot"
dir.create(output_dir_box, recursive = TRUE, showWarnings = FALSE)

purrr::walk(unique(boxplot_data$indicator), function(ind) {

  plot_data <- boxplot_data |> filter(indicator == ind)
  ind_label <- unique(plot_data$indicator_name)

  # Overall income group mean reference line (all years pooled)
  income_avg <- plot_data |>
    dplyr::group_by(income_group) |>
    dplyr::summarise(income_mean = mean(value, na.rm = TRUE), .groups = "drop")

  # Per-year median per income group — diamond colour from where median falls
  year_income_median <- plot_data |>
    dplyr::group_by(income_group, year) |>
    dplyr::summarise(year_median = median(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      grp = dplyr::case_when(
        year_median >= 0.75 ~ "A: Extensive",
        year_median >= 0.50 ~ "B: Significant",
        year_median >= 0.25 ~ "C: Medium",
        TRUE                ~ "D: Low"
      ),
      grp = factor(grp, levels = c("A: Extensive", "B: Significant", "C: Medium", "D: Low"))
    )

  p <- ggplot(plot_data, aes(x = year, y = value)) +
    # Background tier bands
    geom_rect(
      data        = gtmi_bands,
      inherit.aes = FALSE,
      aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = fill),
      alpha       = 0.25
    ) +
    scale_fill_identity() +
    # Overall income group mean reference line
    geom_hline(
      data      = income_avg,
      aes(yintercept = income_mean),
      color     = "grey30",
      linetype  = "dashed",
      linewidth = 0.4
    ) +
    # Neutral grey boxes
    geom_boxplot(
      width         = 0.35,
      outlier.shape = NA,
      fill          = "grey85",
      color         = "grey40",
      alpha         = 0.6,
      linewidth     = 0.5
    ) +
    # Country-level jitter — neutral
    geom_jitter(
      color = "grey50",
      size  = 1.2,
      alpha = 0.45,
      width = 0.15
    ) +
    # Per-year median diamond coloured by grp
    geom_point(
      data   = year_income_median,
      aes(x = year, y = year_median, color = grp),
      shape  = 23,
      size   = 4,
      fill   = NA,
      stroke = 1.8
    ) +
    scale_color_manual(
      values = c(
        "A: Extensive" = "#4DAF4A",
        "B: Significant" = "#377EB8",
        "C: Medium" = "#FF7F00",
        "D: Low" = "#E41A1C"
      ),
      name = "GTMI Group"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    facet_wrap(~ income_group, nrow = 1) +
    labs(
      title    = glue("{ind_label}"),
      subtitle = "Distribution of country scores by year and income group",
      x        = "Year",
      y        = "Score (0–1)",
      caption  = paste0(
        "Diamond outline = year median (coloured by GTMI group). ",
        "Dashed line = overall income group mean (all years pooled).\n",
        "Background bands: A ≥0.75 (green), B 0.50–0.75 (blue), ",
        "C 0.25–0.50 (orange), D <0.25 (red)."
      )
    ) +
    theme(
      legend.position  = "bottom",
      strip.text       = element_text(size = 9),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border     = element_blank()
    )

  ggplot2::ggsave(
    filename = file.path(output_dir_box, glue("final_v_abcd_boxplot_{ind}_income.png")),
    plot     = p,
    width    = 12, height = 6, dpi = 300, bg = "white"
  )
})
# Regional & income-group GovTech analysis ────────────────────────────────────
# Outputs:
#   analysis/figs/timet_avg/regional_avgs.png
#   analysis/figs/timet_avg/income_group_avgs.png
#   analysis/figs/boxplot/v5/v5_focus_areas.png


# ── Libraries ─────────────────────────────────────────────────────────────────

library(tidyverse)
library(here)

devtools::load_all()

ggsave_v3 <- partial(
  ggplot2::ggsave,
  bg = "white",
  width = 20,
  height = 6
)


ggsave_v5 <- partial(
  ggplot2::ggsave,
  bg = "white",
  width = 10,
  height = 20
)


# ── Global theme ──────────────────────────────────────────────────────────────

theme_set(
  theme_minimal() +
    theme(
      text             = element_text(size = 16, family = "Segoe UI Semibold"),
      axis.text.x      = element_text(size = 14, hjust = .5),
      axis.text.y      = element_text(size = 16),
      plot.title       = element_text(size = 22, face = "bold"),
      plot.subtitle    = element_text(size = 16),
      plot.background  = element_blank(),
      plot.caption     = element_text(hjust = 0, size = 12),
      panel.grid.minor = element_blank(),
      panel.border     = element_blank()
    )
)


# ── Constants ─────────────────────────────────────────────────────────────────

grp_colors <- c(
  "A" = "#4DAF4A",
  "B" = "#377EB8",
  "C" = "#FF7F00",
  "D" = "#E41A1C"
)

grp_labels <- c(
  "A" = "A  (>=0.75)",
  "B" = "B  (0.50-0.75)",
  "C" = "C  (0.25-0.50)",
  "D" = "D  (<0.25)"
)

income_levels <- c(
  "High income",
  "Upper middle income",
  "Lower middle income",
  "Low income"
)

# Indicator lookup -- keys match raw column names (no _avg suffix)
indicator_labels <- tribble(
  ~indicator, ~indicator_name,
  "gtmi",     "GovTech Maturity Index (GTMI)",
  "cgsi",     "Core Government Systems Index (CGSI)",
  "psdi",     "Public Services Delivery Index (PSDI)",
  "dcei",     "Data & Cybersecurity Index (DCEI)",
  "gtei",     "GovTech Enabler Index (GTEI)"
)

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


# ── Load & join ───────────────────────────────────────────────────────────────

country_class <- cliaretl::wb_income_and_region

# 2025 group assignment per country
grp_lookup <- gtmi_data |>
  filter(year == 2025) |>
  select(country_code, grp)

# Single panel with region, income_group, and 2025 grp
gtmi_panel <- gtmi_data |>
  select(-grp) |>
  left_join(
    country_class |> select(country_code, income_group, region),
    by = "country_code"
  ) |>
  left_join(grp_lookup, by = "country_code") |>
  mutate(
    region = recode_region(region),
    grp    = factor(grp, levels = c("A", "B", "C", "D"))
  )



# ── Boxplot base data ─────────────────────────────────────────────────────────

boxplot_data <- gtmi_panel |>
  filter(year %in% c(2020, 2022, 2025), !is.na(region), !is.na(grp)) |>
  pivot_longer(c(gtmi, cgsi, psdi, dcei, gtei),
               names_to = "indicator", values_to = "value") |>
  filter(!is.na(value)) |>
  left_join(indicator_labels, by = "indicator") |>
  mutate(year = factor(year))


# ── v3: one plot per indicator, faceted by region or income_group ─────────────

output_dir_v3 <- here("analysis", "figs", "boxplot", "v3")
dir.create(output_dir_v3, recursive = TRUE, showWarnings = FALSE)

build_median <- function(df, facet_var) {
  df |>
    group_by(across(all_of(c(facet_var, "year")))) |>
    summarise(year_median = median(value, na.rm = TRUE), .groups = "drop") |>
    mutate(
      grp = factor(case_when(
        year_median >= 0.75 ~ "A",
        year_median >= 0.50 ~ "B",
        year_median >= 0.25 ~ "C",
        TRUE                ~ "D"
      ), levels = c("A", "B", "C", "D"))
    )
}

build_mean <- function(df, facet_var) {
  df |>
    group_by(across(all_of(facet_var))) |>
    summarise(region_mean = mean(value, na.rm = TRUE), .groups = "drop")
}

build_facet_order <- function(df, facet_var) {
  df |>
    group_by(across(all_of(facet_var))) |>
    summarise(overall_median = median(value, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(overall_median)) |>
    pull(!!sym(facet_var))
}

build_v3_plot <- function(plot_data, facet_var, facet_nrow = 1, ind_label) {

  plot_data <- plot_data |>
    filter(!is.na(.data[[facet_var]]))

  facet_order <- build_facet_order(plot_data, facet_var)
  plot_data   <- plot_data |>
    mutate(across(all_of(facet_var), \(x) factor(x, levels = facet_order)))

  med_df  <- build_median(plot_data, facet_var) |>
    mutate(across(all_of(facet_var), \(x) factor(x, levels = facet_order)))
  mean_df <- build_mean(plot_data, facet_var) |>
    mutate(across(all_of(facet_var), \(x) factor(x, levels = facet_order)))

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
        x     = x_pos - 0.15, xend = x_pos + 0.15,
        y     = year_median,  yend = year_median,
        color = grp
      ),
      linewidth = 2.0,
      lineend   = "round"
    ) +

    scale_color_manual(
      values = grp_colors,
      labels = grp_labels,
      name   = "GTMI Group"
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.50, 0.75, 1)
    ) +

    facet_wrap(as.formula(paste("~", facet_var)), nrow = facet_nrow,
               scales = "fixed") +

    labs(
      x        = NULL,
      y        = "Score (0-1)"
    ) +

    theme_minimal(base_size = 10) +
    theme(
      legend.position    = "top",
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
                                        margin = margin(b = 4))
    )
}

purrr::walk(unique(boxplot_data$indicator), function(ind) {

  plot_data <- boxplot_data |> filter(indicator == ind)
  ind_label <- unique(plot_data$indicator_name)

  p_region <- build_v3_plot(plot_data, "region",       facet_nrow = 1, ind_label)
  p_income <- build_v3_plot(plot_data, "income_group", facet_nrow = 1, ind_label)

  ggsave_v3(file.path(output_dir_v3, glue::glue("v3_{ind}_region.png")),
         plot = p_region)

  ggsave_v3(file.path(output_dir_v3, glue::glue("v3_{ind}_income_group.png")),
         plot = p_income)
})


# ── v5: 4 focus-area rows ─────────────────────────────────────────────────────
# One row per sub-index (CGSI, PSDI, DCEI, GTEI); GTMI composite excluded
# x = year  y = score  dots + median tick coloured by 2025 GTMI group

output_dir_v5 <- here("analysis", "figs", "boxplot", "v5")
dir.create(output_dir_v5, recursive = TRUE, showWarnings = FALSE)

v5_data <- boxplot_data |>
  filter(indicator != "gtmi")

# Row order: descending overall median
ind_order <- v5_data |>
  group_by(indicator_name) |>
  summarise(med = median(value, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(med)) |>
  pull(indicator_name)

v5_data <- v5_data |>
  mutate(indicator_name = factor(indicator_name, levels = ind_order))

# Median per indicator x year -- coloured tick
med_v5 <- v5_data |>
  group_by(indicator_name, year) |>
  summarise(year_median = median(value, na.rm = TRUE), .groups = "drop") |>
  mutate(
    grp = factor(case_when(
      year_median >= 0.75 ~ "A",
      year_median >= 0.50 ~ "B",
      year_median >= 0.25 ~ "C",
      TRUE                ~ "D"
    ), levels = c("A", "B", "C", "D")),
    x_pos = as.numeric(year)
  )

# Pooled mean per indicator -- dashed reference line
mean_v5 <- v5_data |>
  group_by(indicator_name) |>
  summarise(overall_mean = mean(value, na.rm = TRUE), .groups = "drop")


p_v5 <- ggplot(v5_data, aes(x = year, y = value)) +

  # Pooled-mean reference
  geom_hline(
    data      = mean_v5,
    aes(yintercept = overall_mean),
    color     = "grey65",
    linetype  = "dashed",
    linewidth = 0.35
  ) +

  # IQR shell; outliers suppressed (visible via jitter)
  geom_boxplot(
    width         = 0.35,
    outlier.shape = NA,
    fill          = NA,
    color         = "grey50",
    linewidth     = 0.4,
    fatten        = 0       # median bar omitted; drawn as coloured tick below
  ) +

  # Country dots coloured by GTMI group
  geom_jitter(
    aes(color = grp),
    width  = 0.09,
    height = 0,
    size   = 1.8,
    alpha  = 0.55,
    stroke = 0
  ) +

  # Coloured median tick
  geom_segment(
    data = med_v5,
    aes(
      x     = x_pos - 0.175, xend = x_pos + 0.175,
      y     = year_median,   yend = year_median,
      color = grp
    ),
    linewidth = 2.2,
    lineend   = "round"
  ) +

  scale_color_manual(
    values = grp_colors,
    labels = grp_labels,
    name   = "GTMI Group (2025)"
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.50, 0.75, 1),
    labels = c("0", ".25", ".50", ".75", "1")
  ) +

  facet_wrap(~indicator_name, ncol = 1) +   # 4 rows, one per sub-index

  labs(
    title    = "GovTech Sub-Index Scores by Year",
    subtitle = paste0(
      "Country-level scores across the four GTMI focus areas",
      " -- 2020, 2022 and 2025\n",
      "Coloured tick = group median  .  Dashed line = pooled mean"
    ),
    x = NULL,
    y = "Score (0-1)"
  ) +

  theme_minimal(base_size = 10) +
  theme(
    legend.position    = "top",
    legend.direction   = "horizontal",
    legend.key.size    = unit(0.4, "cm"),
    legend.text        = element_text(size = 8),
    legend.title       = element_text(size = 8.5, face = "bold"),
    strip.text         = element_text(size = 9, face = "bold"),
    strip.background   = element_blank(),
    axis.text.x        = element_text(size = 8.5),
    axis.text.y        = element_text(size = 8),
    axis.title.y       = element_text(size = 8.5, color = "grey45",
                                      margin = margin(r = 4)),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border       = element_blank(),
    panel.spacing      = unit(0.9, "lines"),
    plot.title         = element_text(face = "bold", size = 13,
                                      margin = margin(b = 2)),
    plot.subtitle      = element_text(size = 8.5, color = "grey40",
                                      margin = margin(b = 8)),
    plot.margin        = margin(12, 12, 12, 12)
  )

ggsave_v5(
  file.path(output_dir_v5, "v5_focus_areas.png"),
  plot   = p_v5,
)

message("Saved -> ", file.path(output_dir_v5, "v5_focus_areas.png"))
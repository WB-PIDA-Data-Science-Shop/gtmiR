## ============================================================================
## GTMI: Biggest indicator movers by adoption-level group (2022 -> 2025)
## Replicates the faceted horizontal bar chart (panels A-D), coloured by
## pillar (CGSI / DCEI / GTEI / PSDI), showing the top-5 percentage-point
## (pp) increases and top-3 pp decreases in "full adoption" of each
## GovTech indicator, within each baseline adoption-level group.
## ============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(stringr)
library(readr)
library(purrr)
library(janitor)

ggsave <- partial(
  ggplot2::ggsave,
  bg = "white",
  width = 16,
  height = 18
)



devtools::load_all()


# 0. data-load -----------------------------------------------------------


gtmi_indicators_raw <- gtmi_indicators



metadata <- gtmi_indicator_metadata



# 1. data-prep -----------------------------------------------------------
## a) Baseline adoption-level group ("grp": A/B/C/D) per country
## Countries are tracked longitudinally against their 2022 (baseline) tier,
## so a country's movers are attributed to where it *started*, not where it
## ended up. NOTE: gtmi_data contains a few corrupted 2025 rows (grp values
## like "42"/"80" and a missing country_code) -- these are dropped by
## keeping only valid A-D codes.

adoption_level <- gtmi_data |>
  filter(year == 2025, grp %in% c("A", "B", "C", "D")) |>
  distinct(country_code, grp)


## b) Indicator metadata: pillar, "fully adopted" threshold, short label 
## Only indicators scored on a survey scale (binary 0/1 or progressive
## 0/1/2[/3]) support a "share fully adopted" pp calculation -- the
## external continuous indices (UN/ITU/WB indices) are excluded.
## Indicators flagged `longitudinal_feasibility == "blocked"` measure a
## different construct in 2022 vs 2025 and must never be pooled across
## waves (see the `note` field), so they're dropped too.
## "Fully adopted" = level 2, the highest tier common to nearly all
## progressive indicators (two indicators go up to level 3, but level 2
## is used everywhere for comparability).

ind_meta <- metadata |>
  filter(scoring_method %in% c("progressive", "binary")) |>
  filter(longitudinal_feasibility != "blocked") |>
  mutate(
    max_level   = if_else(scoring_method == "binary", 1, 2),
    ind_num     = as.integer(str_extract(indicator, "\\d+$")),
    short_label = paste0("I-", ind_num, ": ", label_2025)
  ) |>
  select(indicator, pillar, max_level, short_label)

## ---------------------------------------------------------------------------
## c) Reshape indicator scores to long form + flag "fully adopted" ----------
## ---------------------------------------------------------------------------
indicators_long <- gtmi_indicators_raw |>
  filter(year %in% c(2022, 2025)) |>
  select(country_code, year, starts_with("wb_gtmi_i_")) |>
  pivot_longer(starts_with("wb_gtmi_i_"), names_to = "indicator", values_to = "score") |>
  inner_join(ind_meta, by = "indicator") |>
  filter(!is.na(score)) |>
  mutate(fully_adopted = score >= max_level)

## ---------------------------------------------------------------------------
## d) Share (%) of countries fully adopted, by group x indicator x year -----
## ---------------------------------------------------------------------------
pct_adopted <- indicators_long |>
  inner_join(adoption_level, by = "country_code") |>
  group_by(grp, indicator, short_label, pillar, year) |>
  summarise(pct_fully_adopted = 100 * mean(fully_adopted), n = n(), .groups = "drop")

## ---------------------------------------------------------------------------
## e) Percentage-point (pp) change, 2022 -> 2025 -----------------------------
## ---------------------------------------------------------------------------
pp_change <- pct_adopted |>
  select(grp, indicator, short_label, pillar, year, pct_fully_adopted) |>
  pivot_wider(names_from = year, values_from = pct_fully_adopted, names_prefix = "y") |>
  filter(!is.na(y2022), !is.na(y2025), !is.na(pillar)) |>
  mutate(pp_change = y2025 - y2022)

## ---------------------------------------------------------------------------
## f) Per group: top-5 pp increases + top-3 pp decreases, by magnitude ------
## ---------------------------------------------------------------------------
top_movers <- pp_change |>
  group_by(grp) |>
  group_modify(~ bind_rows(
    slice_max(.x, order_by = pp_change, n = 5),
    slice_min(.x, order_by = pp_change, n = 3)
  )) |>
  ungroup() |>
  mutate(direction = if_else(pp_change >= 0, "increase", "decrease"))

## ---------------------------------------------------------------------------
## g) Facet-safe bar ordering (largest positive -> largest negative, top-down)
## Each facet panel needs its own bar order, so bars are placed on a plain
## numeric axis (row_id) and the axis text is supplied via a lookup table --
## this avoids duplicate-label collisions that a shared discrete/factor
## scale would otherwise cause across facets ("free_y" reordering trick).
## ---------------------------------------------------------------------------
top_movers <- top_movers |>
  group_by(grp) |>
  arrange(
    grp,
    desc(pp_change > 0),       # positives block first, negatives block second
    if_else(pp_change > 0,
            -abs(pp_change),   # positives: largest magnitude first (unchanged behavior)
            abs(pp_change))    # negatives: smallest magnitude first (the fix)
  ) |>
  mutate(y_pos = row_number()) |>
  ungroup() |>
  mutate(
    y_key = paste(grp, y_pos, sep = "_"),
    y_lab = fct_reorder(y_key, -y_pos)
  )

# labels keyed by the SAME unique key, not by row_id
axis_lookup <- setNames(str_wrap(top_movers$short_label, width = 25), top_movers$y_key)

# 3-Plotting: colors, labels, axis padding ---------------------------------

grp_labels <- c("A" = "A: Extensive", "B" = "B: Significant",
                "C" = "C: Medium",    "D" = "D: Low")

pillar_colors <- c(cgsi = "#4472C4", dcei = "#70AD47", gtei = "#BF9000", psdi = "#C0325B")
pillar_labels <- c(cgsi = "CGSI",    dcei = "DCEI",    gtei = "GTEI",    psdi = "PSDI")

# Labels readability
rng <- range(top_movers$pp_change, na.rm = TRUE)
pad <- diff(rng) * 0.18

#Plot
p <- ggplot(top_movers, aes(x = pp_change, y = y_lab)) +
  # per-row shading — no separate rect table, no min/max assumptions
  geom_tile(data = filter(top_movers, direction == "decrease"),
            aes(y = y_lab), width = Inf, height = 1,
            fill = "#fbe1e1", alpha = 0.6) +
  geom_col(aes(fill = pillar), width = 0.65) +
  geom_vline(xintercept = 0, colour = "grey35", linewidth = 0.4) +
  geom_text(aes(label = paste0(if_else(pp_change >= 0, "+", ""), round(pp_change, 1), "pp"),
                hjust = if_else(pp_change >= 0, -0.15, 1.15)),
            size = 3, colour = "grey20", fontface = "bold") +
  scale_y_discrete(breaks = top_movers$y_key, labels = axis_lookup) +
  scale_fill_manual(values = pillar_colors, labels = pillar_labels, name = NULL) +
  scale_x_continuous(
  limits = c(-75, 75),
  breaks = seq(-75, 75, by = 25),
  expand = expansion(mult = 0.05)
  ) +
  facet_wrap(~grp, ncol = 1, scales = "free_y",
             labeller = labeller(grp = grp_labels)) +
  labs(
    x = NULL, y = NULL,
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, margin = margin(b = 4)),
    plot.subtitle    = element_text(colour = "grey35", size = 10, margin = margin(b = 10)),
    plot.caption     = element_text(colour = "grey55", size = 8, hjust = 0, margin = margin(t = 8)),
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text       = element_text(face = "bold", hjust = 0, size = 11, margin = margin(t = 6, b = 6, l = 8)),
    panel.spacing.y  = unit(1, "lines"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text.y      = element_text(size = 8.5, lineheight = 0.85, colour = "grey20"),
    axis.text.x      = element_text(size = 9),
    axis.ticks       = element_blank(),
    legend.position      = "top",
    legend.margin    = margin(b = 8),
    plot.margin      = margin(10, 16, 10, 10)
  )

print(p)


ggsave(here::here( 
  "analysis",
        "figs",
        "movers", 
        "gtmi_movers.png")
)




# slide-facet ------------------------------------------------------------

ggsave_wide <- partial(
  ggplot2::ggsave,
  bg = "white",
  width = 22, #18 is good
  height = 10
)

ggplot(top_movers, aes(x = pp_change, y = y_lab)) +
  # per-row shading — no separate rect table, no min/max assumptions
  geom_tile(data = filter(top_movers, direction == "decrease"),
            aes(y = y_lab), width = Inf, height = 1,
            fill = "#fbe1e1", alpha = 0.6) +
  geom_col(aes(fill = pillar), width = 0.65) +
  geom_vline(xintercept = 0, colour = "grey35", linewidth = 0.4) +
  geom_text(aes(label = paste0(if_else(pp_change >= 0, "+", ""), round(pp_change, 1), "pp"),
                hjust = if_else(pp_change >= 0, -0.15, 1.15)),
            size = 3, colour = "grey20", fontface = "bold") +
  scale_y_discrete(breaks = top_movers$y_key, labels = axis_lookup) +
  scale_fill_manual(values = pillar_colors, labels = pillar_labels, name = NULL) +
  scale_x_continuous(
  limits = c(-75, 75),
  breaks = seq(-75, 75, by = 25),
  expand = expansion(mult = 0.05)
  ) +
  facet_wrap(~grp, ncol = 2, scales = "free_y",
             labeller = labeller(grp = grp_labels)) +
  labs(
    x = NULL, y = NULL,
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, margin = margin(b = 4)),
    plot.subtitle    = element_text(colour = "grey35", size = 10, margin = margin(b = 10)),
    plot.caption     = element_text(colour = "grey55", size = 8, hjust = 0, margin = margin(t = 8)),
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text       = element_text(face = "bold", hjust = 0, size = 11, margin = margin(t = 6, b = 6, l = 8)),
    panel.spacing.y  = unit(1, "lines"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text.y      = element_text(size = 8.5, lineheight = 0.85, colour = "grey20"),
    axis.text.x      = element_text(size = 9),
    axis.ticks       = element_blank(),
    legend.position      = "top",
    legend.margin    = margin(b = 8),
    plot.margin      = margin(10, 16, 10, 10)
  )


ggsave_wide(here::here( 
  "analysis",
        "figs",
        "movers", 
        "gtmi_movers_wide.png")
)
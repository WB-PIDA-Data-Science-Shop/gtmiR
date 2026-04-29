# Global avgs by region
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

ggsave <- partial(
  ggplot2::ggsave,
  bg = "white",
  width = 14,
  height = 8
)


# load-data --------------------------------------------------------------

devtools::load_all()

groups_data <- gtmi2025 |> # Lazy data cleaned in raw-data/0.gtmi_data_cleaning.R
  select(-grp)

country_class <- cliaretl::wb_income_and_region



# region-data-transf ------------------------------------------------------------
# Compute regional averages for GTMI indicators in 2020, 2022 and 2025 and their differences

gtmi_panel<- groups_data |>
  left_join(country_class |> select(country_code, income_group, region), by = "country_code") |> 
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
  "cgsi_avg", "Citizen Engagement & Service Index (CGSI)",
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

gtmi_panel<- groups_data |>
  left_join(country_class |> select(country_code, income_group, region), by = "country_code") |> 
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








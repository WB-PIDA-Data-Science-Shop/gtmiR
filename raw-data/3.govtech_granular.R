## code to prepare `gtmi_indicators` dataset goes here
# Source: 2025 GovTech Dataset — GTMI_Data sheet (long format, all years in rows)
# available at: https://datacatalog.worldbank.org/int/search/dataset/0037889/govtech-dataset
# Last access: 05/08/2026
#
# Inputs for creating the metadata are contained in the documentaion directory
#
# Missingness taxonomy (critical for longitudinal analysis):
#   "New"  — indicator not yet in instrument for that wave → structurally missing by design
#            Do NOT impute. Exclude wave for that indicator in longitudinal models.
#   "-" sub-type A (permanent) — 5 territories excluded from UN/ITU external indices
#            (I-16, I-18, I-27, I-43, I-44): HKG, MAC, TWN, PSE, XKX → NA all years
#   "-" sub-type B (wave dropout) — Nicaragua 2025: 43 dashes across all survey items;
#            5 numeric values in 2025 are auto-filled external indices, not survey responses.
#            Flag wave_dropout = TRUE, do not treat as valid survey data.
#   "-" sub-type C (indicator not deployed) — I-26: all 198 countries have "-" in 2022.
#            Question was designed but never deployed. Demotes I-26 from 2yr to 1yr (2025 only).
#
# Blocked indicators (never pool across years):
#   I-40: 2020/2022 = "National ID system"; 2025 = "GreenTech/GovTech policy"
#   I-41: 2020/2022 = "National ID digitized records"; 2025 = "AI ethical guidelines"
#   Both are retained in the panel with longitudinal_feasibility = "blocked".
#
# Output: gtmi_indicators — wide panel, one row per country × year
#         Columns: year, code, economy, wave_dropout, wb_gtmi_i_1 … wb_gtmi_i_48
#         feasibility_flags — lookup table with longitudinal status per indicator


# set-up -----------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(purrr)


# load-data --------------------------------------------------------------

tmp <- tempfile(fileext = ".xlsx")
download.file(
  "https://datacatalogfiles.worldbank.org/ddh-published/0037889/DR0095721/WBG_GovTech_Dataset_Dec2025.xlsx",
  tmp, mode = "wb"
)

raw <- read_excel(tmp, sheet = "GTMI_Data") |>
  clean_names()

# Verify structure — run interactively to confirm column names if re-running
# names(raw)[1:15]
# raw[1:4, 1:8]


# constants --------------------------------------------------------------

# Sub-type A: territories permanently excluded from external indices
EXCLUDED_TERRITORIES <- c("HKG", "MAC", "TWN", "PSE", "XKX")

# External index indicators (UN/ITU sourced — Sub-type A applies)
EXTERNAL_INDICES <- c("wb_gtmi_i_16", "wb_gtmi_i_18", "wb_gtmi_i_27",
                      "wb_gtmi_i_43", "wb_gtmi_i_44")

# Sub-type B: wave dropout country
WAVE_DROPOUT_CODE <- "NIC"
WAVE_DROPOUT_YEAR <- 2025L


# clean-raw --------------------------------------------------------------

gtmi_data <- raw |>
  # Drop junk rows: Code == "2025" (column header echo) or Code == "3" (footnote row)
  # NOTE: after clean_names(), "Year" → "year", "Code" → "code", "Economy" → "economy"
  #       Adjust if names(raw)[1:3] shows different values
  filter(!code %in% c("2025", "3"), !is.na(code)) |>
  mutate(year = as.integer(year)) |>
  # Select only aggregate I-N columns (not sub-indicators i_1_4, i_1_6 etc.)
  # Regex ^i_\d+$ matches i_1, i_2 … i_48 but NOT i_1_4, i_1_6
  select(year, code, economy,
         matches("^i_\\d+$")) |>
  # Rename i_N → wb_gtmi_i_N
  rename_with(~ paste0("wb_gtmi_", .), matches("^i_\\d+$")) |>
  # Replace "New" and "-" with NA across all indicator columns
  # "New"  = indicator not yet in instrument (structural missing)
  # "-"    = various subtypes handled below after flagging
  mutate(
    across(
      starts_with("wb_gtmi_i_"),
      ~ if_else(. %in% c("New", "-"), NA_character_, as.character(.))
    ),
    across(
      starts_with("wb_gtmi_i_"),
      ~ suppressWarnings(as.numeric(.))
    )
  )


# missingness-sub-type-b -------------------------------------------------
# Nicaragua 2025: flag wave dropout BEFORE applying sub-type A NA rules
# so we can distinguish "excluded by design" from "survey dropout"

gtmi_data <- gtmi_data |>
  mutate(
    wave_dropout = (code == WAVE_DROPOUT_CODE & year == WAVE_DROPOUT_YEAR)
  ) |>
  # For Nicaragua 2025: zero out all survey-sourced indicators
  # (the 5 external index values that auto-filled are still structurally valid
  #  for I-16, I-18, I-27 but NOT for I-43, I-44 which were excluded for NIC)
  mutate(
    across(
      starts_with("wb_gtmi_i_") & !all_of(EXTERNAL_INDICES),
      ~ if_else(wave_dropout, NA_real_, .)
    )
  )


# missingness-sub-type-a -------------------------------------------------
# Permanently excluded territories for external indices — force NA regardless of year

gtmi_data <- gtmi_data |>
  mutate(
    across(
      all_of(EXTERNAL_INDICES),
      ~ if_else(code %in% EXCLUDED_TERRITORIES, NA_real_, .)
    )
  )


# missingness-sub-type-c -------------------------------------------------
# I-26 was never deployed in 2022 — all 198 countries have "-"
# Already converted to NA above, but make the intent explicit:
# Force NA for year == 2022 regardless of any residual values

gtmi_data <- gtmi_data |>
  mutate(
    wb_gtmi_i_26 = if_else(year == 2022L, NA_real_, wb_gtmi_i_26)
  )


# feasibility-flags ------------------------------------------------------

feasibility_flags <- tibble::tribble(
  ~indicator,      ~longitudinal_feasibility, ~label_2022,                                                ~label_2025,                                                ~in_2020, ~note,                                                                          ~pillar,        ~response_scale, ~data_source,      ~scoring_method,   ~progressive_levels, ~source,
  # Interoperability (pillar = NA: cross-cutting infrastructure component)
  "wb_gtmi_i_1",  "3-year",    "Shared cloud platform",                                    "Shared cloud platform",                                    TRUE,  NA_character_,                                                                  NA_character_,  "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_2",  "3-year",    "Government enterprise architecture framework",              "Government enterprise architecture framework",              TRUE,  NA_character_,                                                                  NA_character_,  "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_3",  "3-year",    "Government interoperability framework",                     "Government interoperability framework",                     TRUE,  NA_character_,                                                                  NA_character_,  "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_4",  "2-year",    "Government service bus platform",                           "Government service bus platform",                           FALSE, NA_character_,                                                                  NA_character_,  "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  # CGSI: Core Government Systems Index (I-5 to I-14)
  "wb_gtmi_i_5",  "3-year",    "Operational FMIS",                                         "Operational FMIS",                                         TRUE,  NA_character_,                                                                  "cgsi",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_6",  "3-year",    "TSA supported by FMIS",                                    "TSA supported by FMIS",                                    TRUE,  NA_character_,                                                                  "cgsi",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_7",  "3-year",    "Tax Management Information System",                        "Tax Management Information System",                        TRUE,  NA_character_,                                                                  "cgsi",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_8",  "3-year",    "Customs Management Information System",                    "Customs Management Information System",                    TRUE,  NA_character_,                                                                  "cgsi",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_9",  "3-year",    "HRMIS with self-service portal",                           "HRMIS with self-service portal",                           TRUE,  NA_character_,                                                                  "cgsi",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_10", "3-year",    "Payroll System linked with HRMIS",                         "Payroll System linked with HRMIS",                         TRUE,  NA_character_,                                                                  "cgsi",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_11", "2-year",    "Social Insurance system",                                  "Social Insurance system",                                  FALSE, NA_character_,                                                                  "cgsi",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_12", "3-year",    "e-Procurement portal",                                     "e-Procurement portal",                                     TRUE,  NA_character_,                                                                  "cgsi",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_13", "3-year",    "Debt Management System",                                   "Debt Management System",                                   TRUE,  NA_character_,                                                                  "cgsi",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_14", "3-year",    "Public Investment Management System",                      "Public Investment Management System",                      TRUE,  NA_character_,                                                                  "cgsi",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  # GTEI: GovTech Enabling Index — policy & strategy enablers (I-15, I-17)
  "wb_gtmi_i_15", "3-year",    "Open Source Software policy",                              "Open Source Software policy",                              TRUE,  NA_character_,                                                                  "gtei",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  # Standalone external indices (pillar = NA)
  "wb_gtmi_i_16", "standalone","UN Telecommunication Infrastructure Index",                 "UN Telecommunication Infrastructure Index",                 FALSE, "Sub-type A: HKG/MAC/TWN/PSE/XKX excluded",                                     NA_character_,  "0-1",           "external_index",  "external_index",  NA_integer_,         "UN E-Government Survey",
  "wb_gtmi_i_17", "2-year",    "National strategy on disruptive/innovative technologies",   "National strategy on disruptive/innovative technologies",   FALSE, NA_character_,                                                                  "gtei",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_18", "standalone","UN Online Service Index",                                   "UN Online Service Index",                                   FALSE, "Sub-type A: HKG/MAC/TWN/PSE/XKX excluded",                                     NA_character_,  "0-1",           "external_index",  "external_index",  NA_integer_,         "UN E-Government Survey",
  # PSDI: Public Service Delivery Index (I-19 to I-26)
  "wb_gtmi_i_19", "2-year",    "Online public service portal",                             "Online public service portal",                             FALSE, NA_character_,                                                                  "psdi",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_20", "3-year",    "Tax online service portal",                                "Tax online service portal",                                TRUE,  NA_character_,                                                                  "psdi",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_21", "3-year",    "e-Filing for tax/customs",                                 "e-Filing for tax/customs",                                 TRUE,  NA_character_,                                                                  "psdi",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_22", "3-year",    "e-Payment services",                                       "e-Payment services",                                       TRUE,  NA_character_,                                                                  "psdi",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_23", "3-year",    "Customs online service portal",                            "Customs online service portal",                            TRUE,  NA_character_,                                                                  "psdi",         "0/1/2/3",       "survey",          "progressive",     3L,                  "WB GovTech Survey",
  "wb_gtmi_i_24", "2-year",    "Social Insurance/Pension online portal",                   "Social Insurance/Pension online portal",                   FALSE, NA_character_,                                                                  "psdi",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_25", "2-year",    "Job portal",                                               "Job portal",                                               FALSE, NA_character_,                                                                  "psdi",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_26", "1-year",    NA_character_,                                              "Digital ID (national ID) for online services",             FALSE, "Sub-type C: never deployed in 2022; demoted from 2yr to 1yr",                  "psdi",         "0/1",           "survey",          "binary",          NA_integer_,         "WB GovTech Survey",
  # Standalone external index (pillar = NA)
  "wb_gtmi_i_27", "standalone","Cyber security strategy",                                   "Cyber security strategy",                                   FALSE, "Sub-type A: HKG/MAC/TWN/PSE/XKX excluded",                                     NA_character_,  "0-1",           "external_index",  "external_index",  NA_integer_,         "UN E-Government Survey",
  # DCEI: Digital Citizen Engagement Index (I-28 to I-32)
  "wb_gtmi_i_28", "2-year",    "Open Government portal",                                   "Open Government portal",                                   FALSE, NA_character_,                                                                  "dcei",         "0/1",           "survey",          "binary",          NA_integer_,         "WB GovTech Survey",
  "wb_gtmi_i_29", "2-year",    "Open Data portal",                                         "Open Data portal",                                         FALSE, "Metadata entry issue flagged in 2025",                                          "dcei",         "0/1",           "survey",          "binary",          NA_integer_,         "WB GovTech Survey",
  "wb_gtmi_i_30", "2-year",    "Citizen participation platforms",                          "Citizen participation platforms",                          FALSE, NA_character_,                                                                  "dcei",         "0/1",           "survey",          "binary",          NA_integer_,         "WB GovTech Survey",
  "wb_gtmi_i_31", "2-year",    "Citizen feedback platforms",                               "Citizen feedback platforms",                               FALSE, NA_character_,                                                                  "dcei",         "0/1",           "survey",          "binary",          NA_integer_,         "WB GovTech Survey",
  "wb_gtmi_i_32", "2-year",    "Citizen engagement stats published",                       "Citizen engagement stats published",                       FALSE, NA_character_,                                                                  "dcei",         "0/1",           "survey",          "binary",          NA_integer_,         "WB GovTech Survey",
  # GTEI: GovTech Enabling Index — institutional & legal enablers (I-33 to I-45)
  "wb_gtmi_i_33", "3-year",    "GovTech entity",                                           "GovTech entity",                                           TRUE,  NA_character_,                                                                  "gtei",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_34", "3-year",    "Data governance entity",                                   "Data governance entity",                                   TRUE,  NA_character_,                                                                  "gtei",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_35", "3-year",    "GovTech/Digital Transformation strategy",                  "GovTech/Digital Transformation strategy",                  TRUE,  NA_character_,                                                                  "gtei",         "0/1/2/3",       "survey",          "progressive",     3L,                  "WB GovTech Survey",
  "wb_gtmi_i_36", "3-year",    "Whole-of-government approach",                             "Whole-of-government approach",                             TRUE,  NA_character_,                                                                  "gtei",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_37", "3-year",    "RTI Laws",                                                 "RTI Laws",                                                 TRUE,  NA_character_,                                                                  "gtei",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_38", "3-year",    "Data Protection / Privacy law",                            "Data Protection / Privacy law",                            TRUE,  NA_character_,                                                                  "gtei",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_39", "3-year",    "Data Protection Authority",                                "Data Protection Authority",                                TRUE,  NA_character_,                                                                  "gtei",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_40", "blocked",   "National ID system",                                       "GreenTech/GovTech policy",                                 FALSE, "DIFFERENT CONSTRUCT across years — never pool",                                 "gtei",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_41", "blocked",   "National ID digitized records",                            "AI ethical guidelines",                                    FALSE, "DIFFERENT CONSTRUCT across years — never pool",                                 "gtei",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_42", "2-year",    "Digital signature regulation and PKI",                     "Digital signature regulation and PKI",                     FALSE, NA_character_,                                                                  "gtei",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  # Standalone external indices (pillar = NA)
  "wb_gtmi_i_43", "standalone","ITU Global Cybersecurity Index",                            "ITU Global Cybersecurity Index",                           FALSE, "Sub-type A: HKG/MAC/TWN/PSE/XKX excluded; 0-100 scale, no rescaling needed",  NA_character_,  "0-100",         "external_index",  "external_index",  NA_integer_,         "ITU Global Cybersecurity Index",
  "wb_gtmi_i_44", "standalone","UN Human Capital Index",                                    "UN Human Capital Index",                                   FALSE, "Sub-type A: HKG/MAC/TWN/PSE/XKX excluded",                                     NA_character_,  "0-1",           "external_index",  "external_index",  NA_integer_,         "World Bank Human Capital Index",
  # GTEI continued: skills & innovation (I-45 to I-48)
  "wb_gtmi_i_45", "2-year",    "Digital skills strategy in public sector",                 "Digital skills strategy in public sector",                 FALSE, NA_character_,                                                                  "gtei",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_46", "2-year",    "Public sector innovation strategy",                        "Public sector innovation strategy",                        FALSE, NA_character_,                                                                  "gtei",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_47", "2-year",    "Public sector innovation entity",                          "Public sector innovation entity",                          FALSE, NA_character_,                                                                  "gtei",         "0/1/2",         "survey",          "progressive",     2L,                  "WB GovTech Survey",
  "wb_gtmi_i_48", "2-year",    "GovTech startup support policy",                           "GovTech startup support policy",                           FALSE, NA_character_,                                                                  "gtei",         "0/1",           "survey",          "binary",          NA_integer_,         "WB GovTech Survey"
)


# build-panel ------------------------------------------------------------

gtmi_indicators <- gtmi_data |>
  select(year, country_code = code, economy, wave_dropout,
         any_of(paste0("wb_gtmi_i_", 1:48))) |>
  arrange(country_code, year)


# validate ---------------------------------------------------------------

# 1. Row counts per year (expect ~198 each)
gtmi_indicators |> count(year) |> print()

# 2. Sub-type B: confirm Nicaragua 2025 flagged, survey items are NA
gtmi_indicators |>
  filter(country_code == "NIC") |>
  select(year, wave_dropout, wb_gtmi_i_1, wb_gtmi_i_5, wb_gtmi_i_16) |>
  print()

# 3. Sub-type A: confirm HKG has NA for all external indices across years
gtmi_indicators |>
  filter(country_code == "HKG") |>
  select(year, all_of(EXTERNAL_INDICES)) |>
  print()

# 4. Sub-type C: confirm I-26 is all NA in 2022
gtmi_indicators |>
  filter(year == 2022) |>
  summarise(n_non_na_i26 = sum(!is.na(wb_gtmi_i_26))) |>
  print()

# 5. Feasibility coverage check
feasibility_flags |>
  count(longitudinal_feasibility) |>
  print()

# 6. Warn about blocked indicators
feasibility_flags |>
  filter(longitudinal_feasibility == "blocked") |>
  select(indicator, label_2022, label_2025, note) |>
  print()

# 7. Summary statistics: min, max, mean per indicator
sum_stats <- gtmi_indicators |>
  select(starts_with("wb_gtmi_i_")) |>
  summarise(
    across(
      everything(),
      list(
        min = ~ min(., na.rm = TRUE),
        max = ~ max(., na.rm = TRUE),
        mean = ~ mean(., na.rm = TRUE),
        n_na = ~ sum(is.na(.))
      ),
      .names = "{.col}_{.fn}"
     )
  ) |>
  pivot_longer(
    everything(),
    names_to = "indicator_stat",
    values_to = "value"
  ) |>
  separate(indicator_stat, into = c("indicator", "statistic"), sep = "_(?=[^_]*$)") |>
  pivot_wider(names_from = statistic, values_from = value) |>
  arrange(indicator) |>
  print(n = Inf)

# Indicator rows: NA indicates no. of missing values (should be 0 for 2020/2022 indicators, but up to 5 for 2025 indicators due to Nicaragua dropout and excluded territories)
# Sample rows: NA indicates no. of non-missing values (should be 594 = 198 countries × 3 years)





# Finally, rename 
 gtmi_indicator_metadata <- feasibility_flags

# export -----------------------------------------------------------------
usethis::use_data(gtmi_indicators,   overwrite = TRUE)
usethis::use_data(gtmi_indicator_metadata , overwrite = TRUE)

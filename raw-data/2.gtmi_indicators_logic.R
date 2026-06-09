## code to prepare `gtmi_indicators` dataset goes here
# GovTech Dataset, specifically, indicator level data, downloaded directly from the WB Development Data Hub
# available at: https://datacatalog.worldbank.org/int/search/dataset/0037889/govtech-dataset
# Last access: 05/08/2026
#
# Panel design (from coherence report):
#   "3-year"    — stable indicator, present in 2020 + 2022 + 2025 (24 indicators)
#   "2-year"    — stable indicator, present in 2022 + 2025 only  (17 indicators)
#   "blocked"   — indicator ID recoded to different construct across years (I-40, I-41)
#   "standalone"— external index, not a GTMI survey question (I-16,I-18,I-27,I-43,I-44)
#
# Output: wide format, one row per country × year, columns named wb_gtmi_i_N


# set-up -----------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(here)
library(httr)
library(purrr)


# load-data --------------------------------------------------------------

datasets <- list(
  list(
    url   = "https://datacatalogfiles.worldbank.org/ddh-published/0037889/DR0045921/wbg_dgss-dataset_december2020.xlsx",
    sheet = "DGSS",
    select = NULL,
    name  = "indicators2020_raw"
  ),
  list(
    url   = "https://datacatalogfiles.worldbank.org/ddh-published/0037889/DR0089805/WBG_GovTech%20Dataset_Oct2022.xlsx",
    sheet = "CG_GTMI_Groups",
    select = NULL,
    name  = "govtech2022_raw"
  ),
  list(
    url   = "https://datacatalogfiles.worldbank.org/ddh-published/0037889/DR0095721/WBG_GovTech_Dataset_Dec2025.xlsx",
    sheet = "GTMI_Groups",
    select = NULL,
    name  = "govtech2025_raw"
  )
)

load_gtmi <- function(url, sheet, select = NULL) {
  temp_file <- tempfile(fileext = ".xlsx")
  download.file(url, destfile = temp_file, mode = "wb")
  dat <- read_excel(temp_file, sheet = sheet) |> clean_names()
  if (!is.null(select)) dat <- dat |> select(all_of(select))
  dat
}

for (ds in datasets) {
  assign(ds$name, load_gtmi(ds$url, ds$sheet, ds$select))
}


# crosswalks -------------------------------------------------------------
# 2020: DGSS sheet cols 508-556 use abbreviated names (confirmed from report)
# Named vector: new_name = old_col_name_after_clean_names
# NOTE: verify positional names by running the inspect block above first

crosswalk_2020_id <- c(
  country_code = "x3"  # ISO3 country code, labeled "Code" in DGSS source row 1
)

# Indicator columns in 2020: cols 509-556 use i_N_NNN format after clean_names()
# Rename all 48 to wb_gtmi_i_N; the pipeline will keep only those with in_2020=TRUE
crosswalk_2020_ind <- setNames(
  paste0("i_", 1:48, "_", seq(509, 556)),  # old names (confirmed from inspect)
  paste0("wb_gtmi_i_", 1:48)               # new standard names
)

# 2022 and 2025: indicator columns are named i_1 through i_48 after clean_names()
# Rename to wb_gtmi_i_N for consistency
crosswalk_2022_2025_ind <- paste0("i_", 1:48) |>
  setNames(paste0("wb_gtmi_i_", 1:48))

# ID columns in 2022 / 2025
crosswalk_2022_id <- c(country_code = "code",   country_name = "economy")
crosswalk_2025_id <- c(country_code = "code_2", country_name = "economy_3")


# longitudinal-feasibility-flags -----------------------------------------

feasibility_flags <- tibble::tribble(
  ~indicator,      ~longitudinal_feasibility, ~label_2022,                                                               ~label_2025,                                                               ~in_2020,
  "wb_gtmi_i_1",  "3-year",    "Shared cloud platform",                                                   "Shared cloud platform",                                                   TRUE,
  "wb_gtmi_i_2",  "3-year",    "Government enterprise architecture framework",                             "Government enterprise architecture framework",                             TRUE,
  "wb_gtmi_i_3",  "3-year",    "Government interoperability framework",                                    "Government interoperability framework",                                    TRUE,
  "wb_gtmi_i_4",  "2-year",    "Government service bus platform",                                          "Government service bus platform",                                          FALSE,
  "wb_gtmi_i_5",  "3-year",    "Operational FMIS",                                                        "Operational FMIS",                                                        TRUE,
  "wb_gtmi_i_6",  "3-year",    "TSA supported by FMIS",                                                   "TSA supported by FMIS",                                                   TRUE,
  "wb_gtmi_i_7",  "3-year",    "Tax Management Information System",                                       "Tax Management Information System",                                       TRUE,
  "wb_gtmi_i_8",  "3-year",    "Customs Management Information System",                                   "Customs Management Information System",                                   TRUE,
  "wb_gtmi_i_9",  "3-year",    "HRMIS with self-service portal",                                          "HRMIS with self-service portal",                                          TRUE,
  "wb_gtmi_i_10", "3-year",    "Payroll System linked with HRMIS",                                        "Payroll System linked with HRMIS",                                        TRUE,
  "wb_gtmi_i_11", "2-year",    "Social Insurance system",                                                 "Social Insurance system",                                                 FALSE,
  "wb_gtmi_i_12", "3-year",    "e-Procurement portal",                                                    "e-Procurement portal",                                                    TRUE,
  "wb_gtmi_i_13", "3-year",    "Debt Management System",                                                  "Debt Management System",                                                  TRUE,
  "wb_gtmi_i_14", "3-year",    "Public Investment Management System",                                     "Public Investment Management System",                                     TRUE,
  "wb_gtmi_i_15", "3-year",    "Open Source Software policy",                                             "Open Source Software policy",                                             TRUE,
  "wb_gtmi_i_16", "standalone","UN Telecommunication Infrastructure Index",                                "UN Telecommunication Infrastructure Index",                                FALSE,
  "wb_gtmi_i_17", "2-year",    "National strategy on disruptive/innovative technologies",                  "National strategy on disruptive/innovative technologies",                  FALSE,
  "wb_gtmi_i_18", "standalone","UN Online Service Index",                                                  "UN Online Service Index",                                                 FALSE,
  "wb_gtmi_i_19", "2-year",    "Online public service portal",                                            "Online public service portal",                                            FALSE,
  "wb_gtmi_i_20", "3-year",    "Tax online service portal",                                               "Tax online service portal",                                               TRUE,
  "wb_gtmi_i_21", "3-year",    "e-Filing for tax/customs",                                                "e-Filing for tax/customs",                                                TRUE,
  "wb_gtmi_i_22", "3-year",    "e-Payment services",                                                      "e-Payment services",                                                      TRUE,
  "wb_gtmi_i_23", "3-year",    "Customs online service portal",                                           "Customs online service portal",                                           TRUE,
  "wb_gtmi_i_24", "2-year",    "Social Insurance/Pension online portal",                                  "Social Insurance/Pension online portal",                                  FALSE,
  "wb_gtmi_i_25", "2-year",    "Job portal",                                                              "Job portal",                                                              FALSE,
  "wb_gtmi_i_26", "2-year",    "Digital ID for online service access",                                    "Digital ID (national ID) for online services",                            FALSE,
  "wb_gtmi_i_27", "standalone","Cyber security strategy",                                                  "Cyber security strategy",                                                 FALSE,
  "wb_gtmi_i_28", "2-year",    "Open Government portal",                                                  "Open Government portal",                                                  FALSE,
  "wb_gtmi_i_29", "2-year",    "Open Data portal",                                                        "Open Data portal [metadata entry issue in 2025]",                         FALSE,
  "wb_gtmi_i_30", "2-year",    "Citizen participation platforms",                                         "Citizen participation platforms",                                         FALSE,
  "wb_gtmi_i_31", "2-year",    "Citizen feedback platforms",                                              "Citizen feedback platforms",                                              FALSE,
  "wb_gtmi_i_32", "2-year",    "Citizen engagement stats published",                                      "Citizen engagement stats published",                                      FALSE,
  "wb_gtmi_i_33", "3-year",    "GovTech entity",                                                          "GovTech entity",                                                          TRUE,
  "wb_gtmi_i_34", "3-year",    "Data governance entity",                                                  "Data governance entity",                                                  TRUE,
  "wb_gtmi_i_35", "3-year",    "GovTech/Digital Transformation strategy",                                 "GovTech/Digital Transformation strategy",                                 TRUE,
  "wb_gtmi_i_36", "3-year",    "Whole-of-government approach",                                            "Whole-of-government approach",                                            TRUE,
  "wb_gtmi_i_37", "3-year",    "RTI Laws",                                                                "RTI Laws",                                                                TRUE,
  "wb_gtmi_i_38", "3-year",    "Data Protection / Privacy law",                                           "Data Protection / Privacy law",                                           TRUE,
  "wb_gtmi_i_39", "3-year",    "Data Protection Authority",                                               "Data Protection Authority",                                               TRUE,
  "wb_gtmi_i_40", "blocked",   "National ID system [2022 meaning]",                                       "GreenTech/GovTech policy [2025 meaning — DIFFERENT CONSTRUCT]",            FALSE,
  "wb_gtmi_i_41", "blocked",   "National ID digitized records [2022 meaning]",                            "AI ethical guidelines [2025 meaning — DIFFERENT CONSTRUCT]",              FALSE,
  "wb_gtmi_i_42", "2-year",    "Digital signature regulation and PKI",                                    "Digital signature regulation and PKI",                                    FALSE,
  "wb_gtmi_i_43", "standalone","ITU Global Cybersecurity Index",                                           "ITU Global Cybersecurity Index",                                          FALSE,
  "wb_gtmi_i_44", "standalone","UN Human Capital Index",                                                   "UN Human Capital Index",                                                  FALSE,
  "wb_gtmi_i_45", "2-year",    "Digital skills strategy in public sector",                                "Digital skills strategy in public sector",                                FALSE,
  "wb_gtmi_i_46", "2-year",    "Public sector innovation strategy",                                       "Public sector innovation strategy",                                       FALSE,
  "wb_gtmi_i_47", "2-year",    "Public sector innovation entity",                                         "Public sector innovation entity",                                         FALSE,
  "wb_gtmi_i_48", "2-year",    "GovTech startup support policy",                                          "GovTech startup support policy",                                          FALSE
)


# standardise-years ------------------------------------------------------

# ── 2020 ─────────────────────────────────────────────────────────────────────
# Row 2 of the DGSS sheet is a sub-header — drop it with slice(-1)
# Cols 509-556 hold i_1_509…i_48_556; col 508 ("gtmi_raw_scores") = country_code
indicators2020 <- indicators2020_raw |>
  slice(-1) |>  # remove extra sub-header row
  select(
    country_code = all_of(crosswalk_2020_id[["country_code"]]),
    all_of(paste0("i_", 1:48, "_", seq(509, 556)))
  ) |>
  rename(all_of(crosswalk_2020_ind)) |>
  # Keep only indicators present in 2020 (in_2020 == TRUE)
  select(country_code,
         any_of(feasibility_flags |> dplyr::filter(in_2020) |> dplyr::pull(indicator))) |>
  mutate(
    year         = 2020L,
    country_code = as.character(country_code),
    across(starts_with("wb_gtmi_i_"), ~ suppressWarnings(as.numeric(.)))
  ) |>
  filter(!is.na(country_code))

# ── 2022 ─────────────────────────────────────────────────────────────────────
indicators2022 <- govtech2022_raw |>
  rename(any_of(setNames(crosswalk_2022_id, names(crosswalk_2022_id)))) |>
  rename(any_of(crosswalk_2022_2025_ind)) |>
  select(country_code, country_name,
         any_of(paste0("wb_gtmi_i_", 1:48))) |>
  mutate(
    year         = 2022L,
    country_code = as.character(country_code),
    across(starts_with("wb_gtmi_i_"), ~ suppressWarnings(as.numeric(.)))
  ) |>
  filter(!is.na(country_code))

# ── 2025 ─────────────────────────────────────────────────────────────────────
indicators2025 <- govtech2025_raw |>
  select(1:min(ncol(govtech2025_raw), 273)) |>  # drop duplicate comparison block
  rename(any_of(setNames(crosswalk_2025_id, names(crosswalk_2025_id)))) |>
  rename(any_of(crosswalk_2022_2025_ind)) |>
  select(country_code, country_name,
         any_of(paste0("wb_gtmi_i_", 1:48))) |>
  mutate(
    year         = 2025L,
    country_code = as.character(country_code),
    across(starts_with("wb_gtmi_i_"), ~ suppressWarnings(as.numeric(.)))
  ) |>
  filter(!is.na(country_code))


# build-panel ------------------------------------------------------------
# Stack all years; missing indicators for a year remain NA

gtmi_indicators_long <- bind_rows(indicators2020, indicators2022, indicators2025) |>
  select(year, country_code, country_name,
         any_of(paste0("wb_gtmi_i_", 1:48))) |>
  arrange(country_code, year)

# Add longitudinal_feasibility as column-level metadata via attr,
# and also as a separate lookup table (feasibility_flags is ready to join)

# Wide format is already the structure (one row per country × year)
gtmi_indicators <- gtmi_indicators_long


# validate ---------------------------------------------------------------

# Coverage per year
gtmi_indicators |>
  count(year) |>
  print()

# Missing values per indicator per year (spot-check 3-year indicators)
three_year_ids <- feasibility_flags |>
  filter(longitudinal_feasibility == "3-year") |>
  pull(indicator)

gtmi_indicators |>
  group_by(year) |>
  summarise(
    across(any_of(three_year_ids), ~ sum(is.na(.)), .names = "na_{.col}")
  ) |>
  tidyr::pivot_longer(-year, names_to = "indicator", values_to = "n_missing") |>
  filter(n_missing > 0) |>
  print()

# Flag blocked indicators clearly — they should not be pooled across years
blocked <- feasibility_flags |>
  filter(longitudinal_feasibility == "blocked") |>
  select(indicator, label_2022, label_2025)

message(
  "\n⚠ BLOCKED indicators (not longitudinally comparable):\n",
  paste0("  ", blocked$indicator, ": 2022='", blocked$label_2022,
         "' | 2025='", blocked$label_2025, "'", collapse = "\n")
)


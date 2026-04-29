## code to prepare `gtmi2025` dataset goes here
# GovTech Dataset downloaded direcltly from the WB Development Data Hub
# available at: https://datacatalog.worldbank.org/int/search/dataset/0037889/govtech-dataset
# Last access: 03/23/2026


# set-up -----------------------------------------------------------------

library(readxl)
library(dplyr)
library(janitor)
library(here)
library(httr)

# load data --------------------------------------------------------------
# Define a list of datasets with their URLs, sheet names, and optional column selection
datasets <- list(
  list(
    url = "https://datacatalogfiles.worldbank.org/ddh-published/0037889/DR0045921/wbg_dgss-dataset_december2020.xlsx",
    sheet = "GTMI",
    select = NULL, # Select only the GTMI group columns
    name = "govtech2020_raw"
  ),
  list(
    url = "https://datacatalogfiles.worldbank.org/ddh-published/0037889/DR0089805/WBG_GovTech%20Dataset_Oct2022.xlsx",
    sheet = "CG_GTMI_Groups",
    select = NULL,
    name = "govtech2022_raw"
  ),
  list(
    url = "https://datacatalogfiles.worldbank.org/ddh-published/0037889/DR0095721/WBG_GovTech_Dataset_Dec2025.xlsx",
    sheet = "GTMI_Groups",
    select = NULL,
    name = "govtech2025_raw"
  )
)

# Function to download, read, clean, and optionally select columns
load_gtmi <- function(url, sheet, select = NULL) {
  temp_file <- tempfile(fileext = ".xlsx")
  download.file(url, destfile = temp_file, mode = "wb")
  dat <- read_excel(temp_file, sheet = sheet) |> clean_names()
  if (!is.null(select)) dat <- dat |> select(all_of(select))
  dat
}

# Load all datasets and assign to variables in the global environment
for (ds in datasets) {
  assign(ds$name, load_gtmi(ds$url, ds$sheet, ds$select))
}

govtech2020_raw  <- govtech2020_raw|> 
  mutate(year = 2020,
          year = as.integer(year)) |> 
  select(year, everything())


# clean data -----------------------------------------------------------
# See exactly what column names exist after clean_names() in each year
list(
  `2020` = govtech2020_raw |> clean_names() |> names(),
  `2022` = govtech2022_raw |> clean_names() |> names(),
  `2025` = govtech2025_raw |> clean_names() |> names()
) # print() to inspect

#  Audit column names across years (run this first, inspect output)

crosswalk_2020 <- c(
  country_code = "code_17",     
  country_name = "economy_16",
  grp          = "grp_18",
  gtmi         = "gtmi_19",
  cgsi         = "cgsi_20",
  psdi         = "psdi_21",
  dcei         = "cei_22",
  gtei         = "gtei_23"
)

crosswalk_2022 <- c(
  country_code = "code",
  country_name = "economy",
  grp          = "grp",
  gtmi         = "gtmi",
  cgsi         = "cgsi",
  psdi         = "psdi",
  dcei         = "dcei",         
  gtei         = "gtei"
)

crosswalk_2025 <- c(
  country_code = "code_2",       
  country_name = "economy_3",
  grp          = "grp",
  gtmi         = "gtmi_5",       
  cgsi         = "cgsi",
  psdi         = "psdi",
  dcei         = "dcei",         
  gtei         = "gtei"
)
# Generic standardisation function — applies crosswalk, adds year
standardise_gtmi <- function(dat, year, crosswalk) {
  dat |>
    clean_names() |>
    # 2025 fix: drop the duplicate comparison block appended after col 273
    select(1:min(ncol(dat), 273)) |>
    rename(any_of(setNames(crosswalk, names(crosswalk)))) |>
    select(all_of(names(crosswalk))) |>
    mutate(
      year         = as.integer(year),
      grp          = as.character(grp),
      country_code = as.character(country_code),
      country_name = as.character(country_name),
      across(c(gtmi, cgsi, psdi, dcei, gtei),
             ~ suppressWarnings(as.numeric(.)))   # NAs from ".." or "n/a" are expected
    ) |>
    # janitor: drop blank rows and header-repeat rows (where country_code is NA)
    remove_empty("rows") |>
    filter(!is.na(country_code), !is.na(gtmi))
}

# Inspect what strings became NA in 2022 indicators
na_quick_test <- govtech2022_raw |>
  clean_names() |>
  select(code, dcei) |>
  filter(!grepl("^[0-9.]*$", as.character(dcei))) |>
  distinct(dcei)


govtech2020 <- standardise_gtmi(govtech2020_raw, 2020, crosswalk_2020)
govtech2022 <- standardise_gtmi(govtech2022_raw, 2022, crosswalk_2022)
govtech2025 <- standardise_gtmi(govtech2025_raw, 2025, crosswalk_2025)


# Stack into panel
gtmi_panel <- bind_rows(govtech2020, govtech2022, govtech2025)

# Validate -------------------------------------------------------

# 6a. Coverage — how many countries per year?
gtmi_panel |> count(year) |> print()

# No duplicate country x year rows
dupes <- gtmi_panel |>
  group_by(country_code, year) |>
  filter(n() > 1)
if (nrow(dupes) > 0) warning("Duplicate country-year rows found!") else cat("✔ No duplicates\n")

# Missing values per indicator per year
gtmi_panel |>
  group_by(year) |>
  summarise(across(c(gtmi, cgsi, psdi, dcei, gtei), ~sum(is.na(.)))) |>
  print() # 5 countries missing GTMI scores in 2025, but all have CGSI/PSDI/CEI/GTEI scores

# Score range sanity check (GTMI scores are 0–1)
gtmi_panel |>
  summarise(across(c(gtmi, cgsi, psdi, dcei, gtei),
                   list(min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE)))) |>
  print()


#rename 
gtmi2025 <- gtmi_panel |> 
  select(year, country_code, country_name, grp, gtmi, cgsi, psdi, dcei, gtei)

# Export ---------------------------------------------------------
usethis::use_data(gtmi2025, overwrite = TRUE)

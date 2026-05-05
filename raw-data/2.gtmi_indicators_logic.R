## code to prepare `indicators2025` dataset goes here
# GovTech Dataset, specifically, indicator level data, downloaded direcltly from the WB Development Data Hub
# available at: https://datacatalog.worldbank.org/int/search/dataset/0037889/govtech-dataset
# Last access: 04/29/2026

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
    sheet = "DGSS",
    select = NULL, 
    name = "indicators2020_raw"
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
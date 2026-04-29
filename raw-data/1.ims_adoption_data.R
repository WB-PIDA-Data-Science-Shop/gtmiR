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

url_2025 <- "https://datacatalogfiles.worldbank.org/ddh-published/0037889/DR0095721/WBG_GovTech_Dataset_Dec2025.xlsx"

temp_file <- tempfile(fileext = ".xlsx")
download.file(url_2025, destfile = temp_file, mode = "wb")

govtech2025_raw <- read_excel(temp_file, sheet = "GTMI_Groups") |>
  clean_names()


# clean data -----------------------------------------------------------

# select only relevant variables
govtech2025_ims <- govtech2025_raw |>
  select(
    country_code = code_2,
    country_name = economy_3,
    FMIS = i_5,       # Operational FMIS in place to support core PFM functions
    # wb_gtmi_i_6 = i_6,       # TSA supported by FMIS to automate payments and bank reconciliation
    TMIS = i_7,       # Tax Management Information System in place
    CMIS = i_8,       # Customs Management Information System in place
    EPMIS = i_12,     # E-Procurement portal in place
    DMIS = i_13,     # Debt Management System (DMS) in place
    PIMIS = i_14      # Public Investment Management System (PIMS) in place
  ) 
  
  
ims_adoption <- govtech2025_ims |>
  mutate(year = 2025) 

# Export ---------------------------------------------------------
usethis::use_data(ims_adoption, overwrite = TRUE)







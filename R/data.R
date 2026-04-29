#' GovTech Maturity Index Panel Dataset (2020, 2022, 2025)
#'
#' A country-level panel dataset combining three waves of the World Bank
#' GovTech Maturity Index (GTMI), covering 2020, 2022, and 2025. Each row
#' represents one country in one survey year. All indicator scores are
#' normalized on a 0–1 scale.
#'
#' @format A data frame with 598 rows and 9 variables:
#' \describe{
#'   \item{year}{Survey wave year. Integer. One of \code{2020}, \code{2022}, or \code{2025}.}
#'   \item{country_code}{ISO 3166-1 alpha-3 country code. Character.}
#'   \item{country_name}{Country name as published in the WB GovTech Dataset. Character.}
#'   \item{grp}{GTMI country grouping tier. Character. Values typically range from
#'     \code{"A"} (highest maturity) to \code{"D"} (lowest maturity).}
#'   \item{gtmi}{GovTech Maturity Index — composite score aggregating all four
#'     pillars. Double. Range: 0–1.}
#'   \item{cgsi}{Core Government Systems Index — measures digitization of core
#'     government systems (PFM, HR, tax, customs). Double. Range: 0–1.}
#'   \item{psdi}{Public Service Delivery Index — measures digital delivery of
#'     public services to citizens and businesses. Double. Range: 0–1.}
#'   \item{dcei}{Digital Citizen Engagement Index — measures digital channels
#'     for citizen engagement and participation. Double. Range: 0–1.}
#'   \item{gtei}{GovTech Enabling Index — measures the enabling environment
#'     for GovTech (legal frameworks, infrastructure, skills). Double. Range: 0–1.}
#' }
#'
#' @source World Bank GovTech Dataset, Development Data Hub.
#'   \url{https://datacatalog.worldbank.org/int/search/dataset/0037889/govtech-dataset}
#'
#' @details
#' The dataset was assembled from three separate Excel releases:
#' \itemize{
#'   \item 2020: \code{wbg_dgss-dataset_december2020.xlsx}, sheet \code{GTMI}
#'   \item 2022: \code{WBG_GovTech Dataset_Oct2022.xlsx}, sheet \code{CG_GTMI_Groups}
#'   \item 2025: \code{WBG_GovTech_Dataset_Dec2025.xlsx}, sheet \code{GTMI_Groups}
#' }
#' Column names were harmonised across waves using a per-year crosswalk.
#' The \code{dcei} indicator was labelled \code{cei} in the 2020 release and
#' \code{dcei} in 2022 and 2025; all waves are stored here under \code{dcei}.
#' Non-numeric placeholder values (e.g. \code{".."}) were coerced to \code{NA}.
#'
"gtmi2025"

#' GovTech Information Management Systems (IMS) Adoption Dataset
#'
#' A dataset containing indicators of Information Management System (IMS) adoption
#' across countries for 2025. The data comes from the World Bank's GovTech GTMI
#' (Government Technology Maturity Index) survey and captures the presence of
#' key public financial management and government service delivery systems.
#'
#' @format A tibble with 241 rows and 9 columns:
#' \describe{
#'   \item{country_code}{ISO 3-letter country code}
#'   \item{country_name}{Country name}
#'   \item{FMIS}{Financial Management Information System - Operational FMIS in place to support core PFM functions}
#'   \item{TMIS}{Tax Management Information System - Tax management system in place}
#'   \item{CMIS}{Customs Management Information System - Customs management system in place}
#'   \item{EPMIS}{E-Procurement Management Information System - E-Procurement portal in place}
#'   \item{DMIS}{Debt Management Information System - Debt Management System (DMS) in place}
#'   \item{PIMIS}{Public Investment Management Information System - Public Investment Management System (PIMS) in place}
#'   \item{year}{Year of data collection (2025)}
#' }
#'
#' @details
#' Each IMS indicator captures the presence (1) or absence (0) of a specific system:
#' - FMIS: Operational Financial Management Information System supporting core PFM functions
#' - TMIS: Tax Management Information System
#' - CMIS: Customs Management Information System
#' - EPMIS: E-Procurement portal
#' - DMIS: Debt Management System
#' - PIMIS: Public Investment Management System
#'
#' Note: TSA (Treasury Single Account) indicator (wb_gtmi_i_6) was excluded from this dataset.
#'
#' @source World Bank Development Data Hub - GovTech Dataset
#' \url{https://datacatalog.worldbank.org/int/search/dataset/0037889/govtech-dataset}
#'
#' }
#'
"ims_adoption"
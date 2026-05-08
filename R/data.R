#' GovTech Composite Indices Panel Dataset (2020, 2022, 2025)
#'
#' A country-level panel dataset of weighted composite indices combining three waves
#' of the World Bank GovTech Maturity Index (GTMI), covering 2020, 2022, and 2025.
#' Each row represents one country in one survey year. All index scores are
#' normalized on a 0–1 scale and aggregate binary survey responses across four
#' pillars of government technology maturity.
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
#' @details
#' ## Focus areas measured
#' \itemize{
#'   \item \strong{CGSI (Core Government Systems)}: Digitization of back-office PFM,
#'     HR, tax, and customs systems (FMIS, HRMIS, Tax MIS, Customs MIS, TSA).
#'   \item \strong{PSDI (Public Service Delivery)}: Digital front-end delivery of
#'     government services to citizens and businesses (e-filing, e-payment, portals).
#'   \item \strong{DCEI (Digital Citizen Engagement)}: Digital channels for public
#'     participation, feedback, and transparency (open data, citizen platforms).
#'   \item \strong{GTEI (GovTech Enabling Index)}: Institutional and legal enablers
#'     (GovTech entity, strategy, data governance, RTI laws, digital security).
#' }
#'
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
#' @source World Bank GovTech Dataset, Development Data Hub.
#'   \url{https://datacatalog.worldbank.org/int/search/dataset/0037889/govtech-dataset}
#'
#' @seealso \code{\link{gtmi_indicators}} for individual binary survey items;
#'   \code{\link{ims_adoption}} for systems adoption subset (2025 only).
#'
"gtmi_data"

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
"ims_adoption"

#' GTMI Granular Indicator Panel Dataset (2020, 2022, 2025)
#'
#' A country-year panel of 48 individual GTMI binary/scored indicators drawn
#' from the 2025 GovTech Dataset (\code{GTMI_Data} sheet), which contains all
#' three survey waves in long format. One row per country per survey year.
#'
#' @format A tibble with ~594 rows and 52 variables:
#' \describe{
#'   \item{year}{Survey wave year. Integer. One of \code{2020}, \code{2022}, or \code{2025}.}
#'   \item{code}{ISO 3166-1 alpha-3 country code. Character.}
#'   \item{economy}{Country name as published in the WB GovTech Dataset. Character.}
#'   \item{wave_dropout}{Logical flag. \code{TRUE} for Nicaragua (\code{NIC}) in 2025,
#'     which did not respond to survey questions. The 5 external index values
#'     present for NIC in 2025 are auto-filled, not survey responses.}
#'   \item{wb_gtmi_i_1}{Shared cloud platform. Binary (0/1). Present in 2020, 2022, 2025.}
#'   \item{wb_gtmi_i_2}{Government enterprise architecture framework. Binary. Present all waves.}
#'   \item{wb_gtmi_i_3}{Government interoperability framework. Binary. Present all waves.}
#'   \item{wb_gtmi_i_4}{Government service bus platform. Binary. Present 2022, 2025 only.}
#'   \item{wb_gtmi_i_5}{Operational FMIS. Binary. Present all waves.}
#'   \item{wb_gtmi_i_6}{TSA supported by FMIS. Binary. Present all waves.}
#'   \item{wb_gtmi_i_7}{Tax Management Information System. Binary. Present all waves.}
#'   \item{wb_gtmi_i_8}{Customs Management Information System. Binary. Present all waves.}
#'   \item{wb_gtmi_i_9}{HRMIS with self-service portal. Binary. Present all waves.}
#'   \item{wb_gtmi_i_10}{Payroll System linked with HRMIS. Binary. Present all waves.}
#'   \item{wb_gtmi_i_11}{Social Insurance system. Binary. Present 2022, 2025 only.}
#'   \item{wb_gtmi_i_12}{e-Procurement portal. Binary. Present all waves.}
#'   \item{wb_gtmi_i_13}{Debt Management System. Binary. Present all waves.}
#'   \item{wb_gtmi_i_14}{Public Investment Management System. Binary. Present all waves.}
#'   \item{wb_gtmi_i_15}{Open Source Software policy. Binary. Present all waves.}
#'   \item{wb_gtmi_i_16}{UN Telecommunication Infrastructure Index. Continuous.
#'     \code{NA} for HKG, MAC, TWN, PSE, XKX (permanently excluded by UN).}
#'   \item{wb_gtmi_i_17}{National strategy on disruptive/innovative technologies. Binary.
#'     Present 2022, 2025 only.}
#'   \item{wb_gtmi_i_18}{UN Online Service Index. Continuous.
#'     \code{NA} for HKG, MAC, TWN, PSE, XKX.}
#'   \item{wb_gtmi_i_19}{Online public service portal. Binary. Present 2022, 2025 only.}
#'   \item{wb_gtmi_i_20}{Tax online service portal. Binary. Present all waves.}
#'   \item{wb_gtmi_i_21}{e-Filing for tax/customs. Binary. Present all waves.}
#'   \item{wb_gtmi_i_22}{e-Payment services. Binary. Present all waves.}
#'   \item{wb_gtmi_i_23}{Customs online service portal. Binary. Present all waves.}
#'   \item{wb_gtmi_i_24}{Social Insurance/Pension online portal. Binary. Present 2022, 2025 only.}
#'   \item{wb_gtmi_i_25}{Job portal. Binary. Present 2022, 2025 only.}
#'   \item{wb_gtmi_i_26}{Digital ID (national ID) for online services. Binary.
#'     \strong{1-year only (2025)}. Question designed but never deployed in 2022;
#'     all 2022 values are \code{NA} by design (Sub-type C missingness).}
#'   \item{wb_gtmi_i_27}{Cyber security strategy. Binary.
#'     \code{NA} for HKG, MAC, TWN, PSE, XKX.}
#'   \item{wb_gtmi_i_28}{Open Government portal. Binary. Present 2022, 2025 only.}
#'   \item{wb_gtmi_i_29}{Open Data portal. Binary. Present 2022, 2025 only.
#'     Metadata entry issue flagged in 2025 wave.}
#'   \item{wb_gtmi_i_30}{Citizen participation platforms. Binary. Present 2022, 2025 only.}
#'   \item{wb_gtmi_i_31}{Citizen feedback platforms. Binary. Present 2022, 2025 only.}
#'   \item{wb_gtmi_i_32}{Citizen engagement statistics published. Binary. Present 2022, 2025 only.}
#'   \item{wb_gtmi_i_33}{GovTech entity. Binary. Present all waves.}
#'   \item{wb_gtmi_i_34}{Data governance entity. Binary. Present all waves.}
#'   \item{wb_gtmi_i_35}{GovTech/Digital Transformation strategy. Binary. Present all waves.}
#'   \item{wb_gtmi_i_36}{Whole-of-government approach. Binary. Present all waves.}
#'   \item{wb_gtmi_i_37}{Right to Information (RTI) Laws. Binary. Present all waves.}
#'   \item{wb_gtmi_i_38}{Data Protection / Privacy law. Binary. Present all waves.}
#'   \item{wb_gtmi_i_39}{Data Protection Authority. Binary. Present all waves.}
#'   \item{wb_gtmi_i_40}{\strong{BLOCKED — do not pool across years.}
#'     2020/2022 = "National ID system"; 2025 = "GreenTech/GovTech policy".
#'     Different constructs measured under the same indicator ID.}
#'   \item{wb_gtmi_i_41}{\strong{BLOCKED — do not pool across years.}
#'     2020/2022 = "National ID digitized records"; 2025 = "AI ethical guidelines".
#'     Different constructs measured under the same indicator ID.}
#'   \item{wb_gtmi_i_42}{Digital signature regulation and PKI. Binary. Present 2022, 2025 only.}
#'   \item{wb_gtmi_i_43}{ITU Global Cybersecurity Index. Continuous, 0–100 scale.
#'     \code{NA} for HKG, MAC, TWN, PSE, XKX. No rescaling applied.}
#'   \item{wb_gtmi_i_44}{UN Human Capital Index. Continuous.
#'     \code{NA} for HKG, MAC, TWN, PSE, XKX.}
#'   \item{wb_gtmi_i_45}{Digital skills strategy in public sector. Binary. Present 2022, 2025 only.}
#'   \item{wb_gtmi_i_46}{Public sector innovation strategy. Binary. Present 2022, 2025 only.}
#'   \item{wb_gtmi_i_47}{Public sector innovation entity. Binary. Present 2022, 2025 only.}
#'   \item{wb_gtmi_i_48}{GovTech startup support policy. Binary. Present 2022, 2025 only.}
#' }
#'
#' @details
#' ## Missingness taxonomy
#' Three structurally distinct types of \code{NA} exist in this dataset:
#' \itemize{
#'   \item \strong{Sub-type A (permanent territorial exclusion)}: Five territories
#'     (HKG, MAC, TWN, PSE, XKX) are permanently excluded from external indices
#'     (I-16, I-18, I-27, I-43, I-44) because UN/ITU do not report values for them.
#'   \item \strong{Sub-type B (wave dropout)}: Nicaragua (\code{NIC}) in 2025 did not
#'     respond to any survey questions. All survey-sourced indicators are \code{NA};
#'     the row is retained and flagged via \code{wave_dropout = TRUE}.
#'   \item \strong{Sub-type C (indicator not deployed)}: I-26 was not deployed in the
#'     2022 survey wave. All 2022 values for \code{wb_gtmi_i_26} are \code{NA} by design.
#' }
#' Source values coded as \code{"New"} (indicator introduced after that wave) and
#' \code{"-"} (various structural missingness) were both converted to \code{NA}.
#'
#' ## Blocked indicators
#' \code{wb_gtmi_i_40} and \code{wb_gtmi_i_41} measure \strong{different constructs}
#' in 2020/2022 versus 2025. Never use these in longitudinal models without subsetting
#' to a single wave. See \code{\link{feasibility_flags}} for full details.
#'
#' @seealso \code{\link{gtmi_indicator_metadata}} for the per-indicator longitudinal
#'   feasibility lookup table.
#'
#' @source World Bank GovTech Dataset, Development Data Hub.
#'   \url{https://datacatalog.worldbank.org/int/search/dataset/0037889/govtech-dataset}
#'   Sheet: \code{GTMI_Data}, file: \code{WBG_GovTech_Dataset_Dec2025.xlsx}
#'
"gtmi_indicators"

#' GTMI Indicator Longitudinal Metadata
#'
#' A lookup table describing the longitudinal comparability status of each of the
#' 48 GTMI indicators (\code{wb_gtmi_i_1} through \code{wb_gtmi_i_48}). Use this
#' table to filter \code{\link{gtmi_indicators}} to the appropriate indicator set
#' before any longitudinal analysis.
#'
#' @format A tibble with 48 rows and 6 variables:
#' \describe{
#'   \item{indicator}{Standard indicator name. Character. Matches column names in
#'     \code{\link{gtmi_indicators}}, e.g. \code{"wb_gtmi_i_1"}.}
#'   \item{longitudinal_feasibility}{Comparability classification. Character. One of:
#'     \itemize{
#'       \item \code{"3-year"} — stable across 2020, 2022, and 2025 (24 indicators)
#'       \item \code{"2-year"} — stable across 2022 and 2025 only (17 indicators)
#'       \item \code{"1-year"} — 2025 only; I-26 never deployed in 2022 (1 indicator)
#'       \item \code{"blocked"} — indicator ID reused for a different construct
#'         across years; never pool (I-40, I-41)
#'       \item \code{"standalone"} — external index, not a GTMI survey question
#'         (I-16, I-18, I-27, I-43, I-44)
#'     }}
#'   \item{label_2022}{Question label as used in the 2022 survey wave. Character.
#'     \code{NA} for indicators not present in 2022.}
#'   \item{label_2025}{Question label as used in the 2025 survey wave. Character.}
#'   \item{in_2020}{Logical. \code{TRUE} if the indicator was part of the 2020 survey
#'     instrument.}
#'   \item{note}{Free-text annotation for special handling requirements. Character.
#'     \code{NA} where no special handling is needed.}
#' }
#'
#' @seealso \code{\link{gtmi_indicators}} for the panel dataset this table describes.
#'
#' @source Derived from the World Bank GTMI coherence report and
#'   \url{https://datacatalog.worldbank.org/int/search/dataset/0037889/govtech-dataset}
#'
"gtmi_indicator_metadata"


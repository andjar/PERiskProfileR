#' PERiskProfileR: Preeclampsia Risk Profiling Tool
#'
#' @description
#' An R implementation of preeclampsia risk calculation models based on
#' Fetal Medicine Foundation (FMF) algorithms. This package provides tools
#' for calculating first-trimester screening risks using maternal factors,
#' biophysical markers (MAP, UtPI), and biochemical markers (PlGF).
#'
#' @details
#' \section{Main Functions}{
#' \itemize{
#'   \item \code{\link{calculate_risk}}: Main entry point for batch risk calculation
#'   \item \code{\link{Pregnancy}}: R6 class representing a single pregnancy
#'   \item \code{\link{RiskModelFMFM2023}}: FMF 2023 competitive risk model
#'   \item \code{\link{RiskModelFMFM2025}}: FMF 2025 updated risk model
#' }
#' }
#'
#' \section{Utility Functions}{
#' \itemize{
#'   \item \code{\link{get_demo_data}}: Access demonstration dataset
#'   \item \code{\link{get_validation_data}}: Access validation datasets
#'   \item \code{\link{risk_to_text}}: Convert probability to "1 in X" format
#'   \item \code{\link{text_to_risk}}: Convert "1 in X" format to probability
#' }
#' }
#'
#' \section{Supported Models}{
#' \describe{
#'   \item{FMF2023}{Based on Tan et al. (2019), supports screening at 11-13+6 weeks}
#'   \item{FMF2025}{Updated model supporting earlier screening from 8 weeks}
#' }
#' }
#'
#' \section{Medical Disclaimer}{
#' The risk calculations provided by this package are for research and
#' educational purposes only. This tool should NOT be used to guide
#' clinical management of patients. The developers do not guarantee
#' the accuracy of the risk scores compared to official FMF tools.
#'
#' This software is independently developed and is NOT affiliated with,
#' endorsed by, or supported by the Fetal Medicine Foundation (FMF).
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom R6 R6Class
#' @importFrom checkmate assertChoice
#' @importFrom checkmate assertNumber
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table :=
#' @importFrom data.table as.data.table
#' @importFrom data.table data.table
#' @importFrom data.table rbindlist
## usethis namespace: end
NULL

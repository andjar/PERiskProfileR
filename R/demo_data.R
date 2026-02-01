#' Retrieve Demo Dataset
#'
#' @description
#' Provides access to a demonstration dataset included in the package for
#' testing and example purposes. The dataset contains sample pregnancy data
#' with all required fields for risk calculation.
#'
#' @return A \code{data.table} object containing the demo dataset with columns
#'   matching the expected input format for \code{\link{calculate_risk}}.
#'
#' @details
#' The dataset is stored as a CSV file in the \code{extdata} directory of the
#' package and includes synthetic patient data suitable for demonstrating
#' the package functionality.
#'
#' @examples
#' \dontrun{
#' # Load the demo dataset
#' demo_data <- get_demo_data()
#' head(demo_data)
#'
#' # Calculate risk for all patients
#' results <- calculate_risk(demo_data, model = "FMF2023", as_list = FALSE)
#' }
#'
#' @seealso \code{\link{get_validation_data}} for validation datasets,
#'   \code{\link{calculate_risk}} for using the data
#'
#' @importFrom data.table fread
#' @export
get_demo_data <- function() {
  return(fread(system.file("extdata", "data_validation.csv", package = "PERiskProfileR")))
}

#' Retrieve Validation Dataset
#'
#' @description
#' Provides access to validation datasets used for verifying the accuracy of
#' risk calculations against the official FMF online calculator. The datasets
#' include both analytical (package-calculated) and online (FMF website)
#' results for comparison.
#'
#' @param model A character string specifying which model's validation data
#'   to retrieve. Must be one of \code{"FMF2023"} or \code{"FMF2025"}.
#'   Defaults to \code{"FMF2023"}.
#'
#' @return A \code{data.table} object containing merged validation data with
#'   columns for:
#'   \itemize{
#'     \item Original patient data (demographic and clinical parameters)
#'     \item Analytical results (calculated by this package): \code{*_analytical}
#'     \item Online results (from FMF calculator): \code{*_online}
#'   }
#'
#' @details
#' The validation datasets allow users to compare this package's output
#' against the official FMF online calculator. Small differences may exist
#' due to rounding or implementation details.
#'
#' @examples
#' \dontrun{
#' # Get FMF2023 validation data
#' val_2023 <- get_validation_data("FMF2023")
#'
#' # Compare analytical vs online risk calculations
#' val_2023[, .(id, risk_analytical, risk_online)]
#' }
#'
#' @seealso \code{\link{get_demo_data}} for demonstration data,
#'   \code{\link{calculate_risk}} for calculating risks
#'
#' @importFrom data.table fread setnames
#' @export
get_validation_data <- function(model = "FMF2023") {

  assertChoice(model, c("FMF2023", "FMF2025"))

  file_list <- list(
    "FMF2023" = list(
      analytical = "data_validation_analytical_FMF2023.csv",
      online     = "data_validation_online_FMF2023.csv"
    ),
    "FMF2025" = list(
      analytical = "data_validation_analytical_FMF2025.csv",
      online     = "data_validation_online_FMF2025.csv"
    )
  )



  if (model == "FMF2023") {
    dfk <- fread(system.file("extdata", file_list[[model]][["analytical"]], package = "PERiskProfileR"))
    setnames(dfk,
             old = c("mom_map", "mom_utpi", "mom_plgf", "prior_risk", "prior_risk_text", "risk", "risk_text"),
             new = c("mom_map_analytical", "mom_utpi_analytical", "mom_plgf_analytical", "prior_risk_analytical", "prior_risk_text_analytical", "risk_analytical", "risk_text_analytical"))

    dfl <- fread(system.file("extdata", file_list[[model]][["online"]], package = "PERiskProfileR"))
    dfl[, prior_risk := text_to_risk(prior_risk_text)]
    dfl[, risk := text_to_risk(risk_text)]

    dff <- merge(
      dfk,
      dfl[, .(id,
              mom_map_online         = mom_map,
              mom_utpi_online        = mom_utpi,
              mom_plgf_online        = mom_plgf_calc,
              prior_risk_text_online = prior_risk_text,
              prior_risk_online      = prior_risk,
              risk_text_online       = risk_text,
              risk_online            = risk)],
      by = "id"
    )
  } else if (model == "FMF2025") {
    dfk <- fread(system.file("extdata", file_list[[model]][["analytical"]], package = "PERiskProfileR"))
    setnames(dfk,
             old = c("mom_map", "mom_utpi", "mom_plgf", "prior_risk", "prior_risk_text", "risk", "risk_text"),
             new = c("mom_map_analytical", "mom_utpi_analytical", "mom_plgf_analytical", "prior_risk_analytical", "prior_risk_text_analytical", "risk_analytical", "risk_text_analytical"))

    dfl <- fread(system.file("extdata", file_list[[model]][["online"]], package = "PERiskProfileR"))

    dff <- merge(
      dfk,
      dfl[, .(id,
              mom_map_online         = as.numeric(mom_map_online),
              mom_utpi_online        = mom_utpi,
              mom_plgf_online        = mom_plgf_calc,
              risk_text_online       = risk_text,
              risk_online)],
      by = "id"
    )
  }



  return(dff)
}

#' Calculate Preeclampsia Risk
#'
#' @description
#' Calculates preeclampsia risk for multiple patients using either a formula-based approach
#' or the Fetal Medicine Foundation's online calculator. This function processes multiple
#' patient records and returns risk scores along with biomarker MoM values.
#'
#' @param data Data frame containing patient data with required columns (see Details)
#' @param id_column String, name of column containing patient IDs (default: "id")
#' @param method Character, calculation method: "formula" or "online" (default: "formula")
#' @param report_as_text Boolean, whether to return risks as text fractions (default: FALSE)
#' @param G Numeric, gestational age cutoff in weeks for risk calculation (default: 37)
#' @param save_responses Boolean, whether to save online calculator responses (default: FALSE)
#' @param response_dir String, directory for saving responses (default: "requests")
#' @param url String, FMF calculator URL (default: "https://fetalmedicine.org/research/assess/preeclampsia/First")
#' @param verbose Boolean, whether to show progress messages (default: TRUE)
#'
#' @return Data frame containing original data plus additional columns:
#'   \itemize{
#'     \item mom_MAP: Mean Arterial Pressure MoM
#'     \item mom_PI: Pulsatility Index MoM
#'     \item mom_PlGF: PlGF MoM
#'     \item risk_prior: Prior risk based on maternal characteristics
#'     \item risk: Final risk incorporating biomarkers
#'   }
#'
#' @details
#' Required input columns are defined by \code{\link{get_expected_columns}}. The function
#' supports two calculation methods:
#' \itemize{
#'   \item formula: Uses local implementation of FMF algorithm
#'   \item online: Submits data to FMF online calculator
#' }
#'
#' For the online method, internet connectivity is required and the function will create
#' a response directory if save_responses is TRUE.
#'
#' @section Warning:
#' This package is NOT intended for clinical use. Risk calculations should be verified
#' using official clinical tools.
#'
#' @examples
#' \dontrun{
#' # Calculate risks using formula method
#' results <- calculate_pe_risk(patient_data, method = "formula")
#'
#' # Calculate risks using online calculator and save responses
#' results <- calculate_pe_risk(
#'   patient_data,
#'   method = "online",
#'   save_responses = TRUE,
#'   response_dir = "calculator_responses"
#' )
#' }
#'
#' @seealso
#' \code{\link{calculate_formula_risk}}, \code{\link{calculate_online_risk}}
#'
#' @importFrom data.table copy :=
#' @export
calculate_pe_risk <- function(data,
                              id_column = "id",
                              method = c("formula", "online"),
                              report_as_text = FALSE,
                              G = 37,
                              save_responses = FALSE,
                              response_dir = "requests",
                              url = "https://fetalmedicine.org/research/assess/preeclampsia/First",
                              verbose = TRUE) {

  method <- match.arg(method)

  warning("Please Note: This package is NOT intended for clinical use!")

  if(!id_column %in% colnames(data)) {
    data$id <- seq(1, nrow(data))
  }

  # Create response directory if needed
  if (method == "online" && save_responses) {
    dir.create(response_dir, showWarnings = FALSE, recursive = TRUE)
  }

  n_rows <- nrow(data)
  df_out <- copy(data)

  # Process each row
  for (i in seq_len(n_rows)) {
    if (verbose) {
      cat(sprintf("\rProcessing row %d of %d", i, n_rows))
    }

    row_data <- row_to_list(data[i,])

    if (method == "formula") {
      risk_scores <- calculate_formula_risk(row_data, G = G, report_as_text = report_as_text)

      df_out[i, mom_MAP := risk_scores$mom_MAP]
      df_out[i, mom_PI := risk_scores$mom_PI]
      df_out[i, mom_PlGF := risk_scores$mom_PlGF]
      df_out[i, risk_prior := risk_scores$risk_prior]
      df_out[i, risk := risk_scores$risk]
    } else if (method == "online") {
      result <- calculate_online_risk(
        row_data,
        save_responses = save_responses,
        response_dir = response_dir,
        url = url,
        row_id = data[[id_column]][i]
      )

      if (result$status == "Success") {
        risk_scores <- extract_risk_scores(result$content, report_as_text = report_as_text)
        df_out[i, mom_MAP := risk_scores$mom_MAP]
        df_out[i, mom_PI := risk_scores$mom_PI]
        df_out[i, mom_PlGF := risk_scores$mom_PlGF]
        df_out[i, risk_prior := risk_scores$risk_prior]
        df_out[i, risk := risk_scores$risk]
      } else {
        warning(sprintf("Failed to calculate risk for row %d: %s", i, result$message))
      }
    }
  }

  if (verbose) cat("\nCalculations complete.\n")
  return(df_out)
}

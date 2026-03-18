#' Calculate Preeclampsia Risk
#'
#' @description
#' A wrapper function that computes preeclampsia risk for a cohort of patients
#' using a specified risk model. Included models are based on the research of
#' the Fetal Medicine Foundation (FMF). It processes data-frame rows into
#' individual pregnancy objects and calculates risk scores accordingly.
#'
#' @param df A data frame where each row represents a single pregnancy and
#'   columns correspond to the required model parameters. See
#'   \code{\link{Pregnancy}} for expected column names.
#' @param model A character string specifying the risk model to use.
#'   Must be one of:
#'   \itemize{
#'     \item \code{"FMF2023"}: FMF 2023 model (default)
#'     \item \code{"FMF2025"}: FMF 2025 updated model
#'     \item \code{"None"}: Empty model (returns NA for risk)
#'   }
#' @param as_list Logical. If \code{TRUE} (default), the function returns a list
#'   of \code{Pregnancy} objects. If \code{FALSE}, it returns a combined data frame.
#' @param G Numeric. The target gestational age in weeks for risk calculation.
#'   Default is 37 (risk of preeclampsia before 37 weeks).
#'
#' @return If \code{as_list = TRUE}, a list of \code{\link{Pregnancy}} R6 objects.
#'   If \code{as_list = FALSE}, a \code{data.table} containing the combined results
#'   with calculated risk scores.
#'
#' @details
#' The function uses the \code{pbapply} package to provide a progress bar during
#' calculation, which is useful for large datasets.
#'
#' Required columns in \code{df} include:
#' \itemize{
#'   \item Pregnancy: \code{twins}, \code{crl} or \code{ga}, \code{ga_at}
#'   \item Maternal: \code{date_of_birth}, \code{height}, \code{weight}, \code{race}
#'   \item History: \code{smoking}, \code{mother_pe}, \code{chronic_hypertension}, \code{conception}
#'   \item Biomarkers: \code{map}, \code{utpi}, \code{plgf} (optional)
#' }
#'
#' @examples
#' \dontrun{
#' # Load demo data
#' df <- get_demo_data()
#'
#' # Calculate risk using FMF2023 model
#' results <- calculate_risk(df, model = "FMF2023", as_list = FALSE)
#'
#' # View risk results
#' results[, .(id, risk, risk_text)]
#'
#' # Calculate risk for delivery before 34 weeks
#' results_34 <- calculate_risk(df, model = "FMF2023", G = 34, as_list = FALSE)
#' }
#'
#' @seealso \code{\link{Pregnancy}} for the Pregnancy class,
#'   \code{\link{RiskModelFMFM2023}} and \code{\link{RiskModelFMFM2025}} for model details,
#'   \code{\link{get_demo_data}} for example data
#'
#' @import data.table R6 checkmate
#'
#' @export
calculate_risk <- function (df, model = "FMF2023", as_list = TRUE, G = 37) {

  assertChoice(model, c("FMF2023", "FMF2025", "None"))

  if (model == "FMF2023") {
    risk_model <- RiskModelFMFM2023$new(G = G)
  } else if (model == "FMF2025") {
    risk_model <- RiskModelFMFM2025$new(G = G)
  } else if (model == "None") {
    risk_model <- RiskModelEmpty$new(G = G)
  }

  res <- pbapply::pblapply(seq_len(nrow(df)), function(i) {
    Pregnancy$new(
      params = as.list(df[i, ]),
      risk_model = risk_model
    )
  })

  if ( as_list == TRUE ) {
    return(res)
  } else {
    return(as_df(res))
  }

}

#' Convert Pregnancy List to Data Table
#'
#' @description
#' An internal helper function that takes a list of `Pregnancy` objects and
#' collapses them into a single `data.table`.
#'
#' @param res A list of `Pregnancy` R6 objects.
#'
#' @return A `data.table` containing the aggregated data from all objects,
#'   with missing columns filled where necessary.
#'
#' @importFrom data.table rbindlist
#' @keywords internal
as_df <- function(res) {
  rbindlist(
    lapply(res, function(k) k$as_df()),
    fill = TRUE
  )
}

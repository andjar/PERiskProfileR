#' Calculate Preeclampsia Risk
#'
#' @description
#' A wrapper function that computes preeclampsia risk for a cohort of patients
#' using a specified risk model. Included models are based on the research of
#' Fetal Medicine Foundation (FMF) models. It processes data-frame rows into
#' individual pregnancy objects and calculates risk scores accordingly.
#'
#' @param df A data frame where each row represents a single pregnancy and
#'   columns correspond to the required model parameters.
#' @param model A character string specifying the risk model to use.
#'   Must be one of: `"FMF2023"`, `"FMF2025"`, or `"None"`.
#'   Defaults to `"FMF2023"`.
#' @param as_list Logical. If `TRUE` (default), the function returns a list
#'   of `Pregnancy` objects. If `FALSE`, it returns a combined data frame.
#' @param G An integer specifying the gestational age (default is 37).
#'
#' @return If `as_list = TRUE`, a list of `Pregnancy` R6 objects.
#'   If `as_list = FALSE`, a `data.table` containing the combined results.
#'
#' @details
#' The function uses the `pbapply` package to provide a progress bar during
#' calculation, which is useful for large datasets. It validates the `model`
#' choice using `checkmate::assertChoice`.
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

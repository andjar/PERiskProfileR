#' Retrieve Demo Dataset
#'
#' This function provides access to a demo dataset included in the package.
#' The dataset is stored as a CSV file in the `extdata` directory of the package.
#'
#' @return A `data.table` object containing the contents of the demo dataset.
#'
#' @examples
#' \dontrun{
#' # Load the demo dataset
#' demo_data <- get_demo_data()
#' head(demo_data)
#' }
#'
#' @importFrom data.table fread
#' @keywords prepare
#' @export
get_demo_data <- function() {
  return(fread(system.file("extdata", "data_validation.csv", package = "PERiskProfileR")))
}

#' Retrieve Validation Dataset
#'
#' This function provides access to a validation dataset included in the package.
#' The dataset is stored as a CSV file in the `extdata` directory of the package.
#'
#' @param modle What dataset to retrieve: `FMF2023` or `FMF2025`
#'
#' @return A `data.table` object containing the contents of the validation dataset.
#'
#' @importFrom data.table fread
#' @keywords prepare
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

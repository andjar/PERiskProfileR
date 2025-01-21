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
  return(fread(system.file("extdata", "demo.csv", package = "PERiskProfileR")))
}

get_demo_data_risks <- function() {
  return(fread(system.file("extdata", "demo_risk.csv", package = "PERiskProfileR")))
}

get_validation_data <- function() {
  dfk <- fread(system.file("extdata", "data_validation_analytical.csv", package = "PERiskProfileR"))
  dfl <- fread(system.file("extdata", "data_validation_online.csv", package = "PERiskProfileR"))
  dff <- merge(
    dfk,
    dfl[, .(id, mom_MAP, mom_PI, mom_PlGF, risk_prior, risk)],
    by = "id"
  )
  dff[, nrisk.x := text_to_risk(risk.x)]
  dff[, nrisk.y := text_to_risk(risk.y)]
  dff[, nrisk_prior.x := text_to_risk(risk_prior.x)]
  dff[, nrisk_prior.y := text_to_risk(risk_prior.y)]

  return(dff)
}

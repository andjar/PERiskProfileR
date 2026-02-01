#' Export Single Pregnancy Object to JSON
#'
#' @description
#' Exports a single \code{\link{Pregnancy}} object to a JSON file formatted
#' for use with external tools (e.g., Python scripts, web applications).
#' The output format maps R parameter names to human-readable labels.
#'
#' @param pregnancy_obj A \code{\link{Pregnancy}} R6 object containing clinical data.
#' @param id A character string representing the patient identifier.
#'   Defaults to \code{"TEST_001"}.
#' @param filename A character string specifying the output file path.
#'   Defaults to \code{"patients.json"}.
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side
#'   effect of writing a JSON file.
#'
#' @details
#' The function maps internal parameter names to standardized output labels:
#' \itemize{
#'   \item Race: "white" \rightarrow "White", "afro-caribbean" \rightarrow "Black", etc.
#'   \item Conception: "spontaneous" \rightarrow "Spontaneous", "ivf" \rightarrow "In vitro fertilization"
#'   \item Diabetes treatment: "insulin" \rightarrow "Insulin", "metformin" \rightarrow "Metformin", etc
#' }
#'
#' @seealso \code{\link{export_pregnancy_batch}} for exporting multiple pregnancies,
#'   \code{\link{Pregnancy}} for the Pregnancy class
#'
#' @importFrom jsonlite write_json
#' @export
export_pregnancy_to_json <- function(pregnancy_obj, id = "TEST_001", filename = "patients.json") {
  p <- pregnancy_obj$params

  map_race <- function(r) {
    switch(r,
           "white"          = "White",
           "afro-caribbean" = "Black",
           "south-asian"    = "South Asian",
           "east-asian"     = "East Asian",
           "mixed"          = "White - Black",
           "White"
    )
  }

  map_conception <- function(c) {
    switch(c,
           "spontaneous"     = "Spontaneous",
           "ovulation drugs" = "Ovulation drugs",
           "ivf"             = "In vitro fertilization",
           "Spontaneous"
    )
  }

  map_diabetes_drugs <- function(d) {
    switch(d,
           "metformin"           = "Metformin",
           "insulin+metformin"   = "Insulin, Metformin",
           "insulin"             = "Insulin",
           "diet"                = "Diet only",
           "no"                  = "",
           ""
    )
  }

  patient_data <- list(
    id               = id,
    dob              = format(p$date_of_birth, "%d/%m/%Y"),
    height           = as.character(p$height),
    weight           = as.character(p$weight),
    ethnicity        = map_race(p$race),
    smoking          = ifelse(p$smoking == "yes", "Yes", "No"),

    dating           = "CRL",
    crl              = as.character(p$crl),
    examination_date = format(p$ga_at, "%d/%m/%Y"),

    conception       = map_conception(p$conception),
    ch               = ifelse(p$chronic_hypertension == "yes", "Yes", "No"),
    dbi              = ifelse(p$diabetes_type_i == "yes", "Yes", "No"),
    dbi_medication   = map_diabetes_drugs(p$diabetes_drugs),
    dbii             = ifelse(p$diabetes_type_ii == "yes", "Yes", "No"),
    dbii_medication  = map_diabetes_drugs(p$diabetes_drugs),
    mother_pe        = ifelse(p$mother_pe == "yes", "Yes", "No"),
    sle              = ifelse(p$sle == "yes", "Yes", "No"),
    aps              = ifelse(p$aps == "yes", "Yes", "No"),

    parity           = ifelse(p$previous == "yes", "Parous", "Nulliparous"),
    previous_pe      = ifelse(p$previous_pe == "yes", "Yes", "No"),
    previous_delivered_at = ifelse(p$previous == "yes", format(p$previous_delivered_at, "%d/%m/%Y"), ""),
    previous_ga_weeks     = p$previous_ga_weeks,
    previous_ga_days      = p$previous_ga_days,

    map_val          = as.character(p$map),
    utapi_val        = as.character(p$utpi),
    plgf_date        = format(p$biochemical_at %||% p$ga_at, "%d/%m/%Y"),
    plgf_val         = as.character(p$plgf),
    plgf_method      = tools::toTitleCase(p$plgf_machine %||% "roche")
  )

  write_json(list(patient_data), filename, auto_unbox = TRUE, pretty = TRUE)
  message(paste("Patient data saved to", filename))
}

#' Export Multiple Pregnancy Objects to JSON
#'
#' @description
#' Exports a list of \code{\link{Pregnancy}} objects to a single JSON file.
#' This is useful for batch processing or transferring multiple patient
#' records to external systems.
#'
#' @param pregnancy_list A list of \code{\link{Pregnancy}} R6 objects.
#' @param ids An optional character vector of patient identifiers. If \code{NULL}
#'   (default), identifiers are auto-generated as "ID_1", "ID_2", etc.
#' @param filename A character string specifying the output file path.
#'   Defaults to \code{"patients.json"}.
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side
#'   effect of writing a JSON file containing an array of patient records.
#'
#' @details
#' Each pregnancy object is converted to a standardized JSON structure with
#' human-readable field names. The same mapping conventions as
#' \code{\link{export_pregnancy_to_json}} are applied.
#'
#' @seealso \code{\link{export_pregnancy_to_json}} for exporting a single pregnancy,
#'   \code{\link{Pregnancy}} for the Pregnancy class,
#'   \code{\link{calculate_risk}} for generating Pregnancy objects from data frames
#'
#' @importFrom jsonlite write_json
#' @export
export_pregnancy_batch <- function(pregnancy_list, ids = NULL, filename = "patients.json") {
  if (is.null(ids)) ids <- paste0("ID_", seq_along(pregnancy_list))

  map_race <- function(r) {
    switch(r, "white"="White", "afro-caribbean"="Black", "south-asian"="South Asian",
           "east-asian"="East Asian", "mixed"="White - Black", "White")
  }
  map_conception <- function(c) {
    switch(c, "spontaneous"="Spontaneous", "ovulation drugs"="Ovulation drugs",
           "ivf"="In vitro fertilization", "Spontaneous")
  }
  map_diabetes_drugs <- function(d) {
    switch(d,
           "metformin"           = "Metformin",
           "insulin+metformin"   = "Insulin, Metformin",
           "insulin"             = "Insulin",
           "diet"                = "Diet only",
           "no"                  = "",
           ""
    )
  }

  all_patients <- lapply(seq_along(pregnancy_list), function(i) {
    preg <- pregnancy_list[[i]]
    p <- preg$params

    list(
      id               = ids[i],
      dob              = format(p$date_of_birth, "%d/%m/%Y"),
      height           = as.character(p$height),
      weight           = as.character(p$weight),
      ethnicity        = map_race(p$race),
      smoking          = ifelse(p$smoking == "yes", "Yes", "No"),
      dating           = "CRL",
      crl              = as.character(p$crl),
      examination_date = format(p$ga_at, "%d/%m/%Y"),
      conception       = map_conception(p$conception),
      ch               = ifelse(p$chronic_hypertension == "yes", "Yes", "No"),
      dbi              = ifelse(p$diabetes_type_i == "yes", "Yes", "No"),
      dbi_medication   = map_diabetes_drugs(p$diabetes_drugs),
      dbii             = ifelse(p$diabetes_type_ii == "yes", "Yes", "No"),
      dbii_medication  = map_diabetes_drugs(p$diabetes_drugs),
      sle              = ifelse(p$sle == "yes", "Yes", "No"),
      aps              = ifelse(p$aps == "yes", "Yes", "No"),
      mother_pe        = ifelse(p$mother_pe == "yes", "Yes", "No"),
      parity           = ifelse(p$previous == "yes", "Parous", "Nulliparous"),
      previous_pe      = ifelse(p$previous_pe == "yes", "Yes", "No"),
      previous_delivered_at = ifelse(p$previous == "yes", format(p$previous_delivered_at, "%d/%m/%Y"), ""),
      previous_ga_weeks     = p$previous_ga_weeks,
      previous_ga_days      = p$previous_ga_days,
      map_val          = as.character(p$map),
      utapi_val        = as.character(p$utpi),
      plgf_date        = format(p$biochemical_at %||% p$ga_at, "%d/%m/%Y"),
      plgf_val         = as.character(p$plgf),
      plgf_method      = tools::toTitleCase(p$plgf_machine %||% "roche")
    )
  })

  jsonlite::write_json(all_patients, filename, auto_unbox = TRUE, pretty = TRUE)
  # message(flog.info("Batch with %s patients saved to %s", length(all_patients), filename))
}

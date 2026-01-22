#' Pregnancy Class
#'
#' @description
#' An R6 class representing a single pregnancy. It stores maternal characteristics,
#' biophysical markers, and biochemical markers, providing a unified interface
#' for preeclampsia risk calculation.
#'
#' @field params A named list containing all clinical parameters and calculated results.
#' @field risk_model An R6 object of class \code{\link{RiskModel}} used for calculations.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(params = list(), risk_model = RiskModelFMFM2023$new())}}{
#'     Constructor for the Pregnancy class. Performs extensive validation on
#'     maternal data, dates, and biomarkers. It automatically calculates
#'     gestational age, maternal age at EDD, expected biomarker values, and
#'     risk scores upon initialization.
#'   }
#'   \item{\code{get(param, truncate_for = "")}}{
#'     Retrieves a parameter value. If \code{truncate_for} is specified (e.g., "risk"),
#'     it returns the value truncated according to the model's limits.
#'   }
#'   \item{\code{get_raw(param)}}{
#'     Retrieves the raw, untruncated value of a parameter.
#'   }
#'   \item{\code{as_df()}}{
#'     Converts the stored parameters and results into a single-row \code{data.table}.
#'   }
#' }
#'
#' @export
Pregnancy <- R6::R6Class(
  classname = "Pregnancy",
  public = list(
    params = NULL,
    risk_model = NULL,

    #' @description Create a new Pregnancy object.
    #' @param params A list containing clinical data. Expected keys include:
    #' \itemize{
    #'   \item \code{crl} or \code{ga}: Crown-rump length (mm) or gestational age (weeks).
    #'   \item \code{ga_at}: Date of dating ultrasound (YYYY-MM-DD).
    #'   \item \code{date_of_birth}: Mother's date of birth (YYYY-MM-DD).
    #'   \item \code{height}, \code{weight}: Maternal height (cm) and weight (kg).
    #'   \item \code{race}: "white", "afro-caribbean", "south-asian", "east-asian", or "mixed".
    #'   \item \code{smoking}, \code{mother_pe}, \code{chronic_hypertension}: "yes" or "no".
    #'   \item \code{conception}: "spontaneous", "ovulation drugs", or "ivf".
    #'   \item \code{map}, \code{utpi}, \code{plgf}: Raw biomarker values.
    #'   \item \code{plgf_machine}: "delfia", "kryptor", or "roche".
    #' }
    #' @param risk_model A \code{RiskModel} object (defaults to \code{RiskModelFMFM2023}).
    initialize = function(

        params = list(),
        risk_model = RiskModelFMFM2023$new()

      ) {

      # Input validations
      # Pregnancy Details
      params$twins <- tolower(params$twins) %||% ""
      assertChoice(params$twins, c("singleton", "monochorionic", "dichorionic"))
      if ( params$twins != "singleton" ) stop("Twins not supported")

      assertNumber(params$crl, na.ok = TRUE)
      assertNumber(params$ga, na.ok = TRUE)
      if ( is.na(params$crl) && is.na(params$ga) ) stop("Provide either GA or CRL")
      if ( !is.na(params$crl) && !is.na(params$ga) ) stop("Provide either GA or CRL")
      if ( is.na(params[["ga"]]) ) {
        params[["ga"]] <- risk_model$get_ga_from_crl(crl = params$crl)
      }

      params[["ga"]] <- floor(params[["ga"]]*7)/7

      if ( is.null(params[["ga_at"]]) ) stop("Please provide a date for the pregnancy dating: 'ga_at'")
      if ( !is.character(params[["ga_at"]]) ) warning("Note: Converting 'ga_at' to date object")
      params[["ga_at"]] <- as.Date(params[["ga_at"]], tryFormats = c("%Y-%m-%d"))

      # Maternal Characteristics
      if ( is.null(params[["date_of_birth"]]) ) stop("Please provide the mother's birthday: 'date_of_birth'")
      if ( !is.character(params[["date_of_birth"]]) ) warning("Note: Converting 'date_of_birth' to date object")
      params[["date_of_birth"]] <- as.Date(params[["date_of_birth"]], tryFormats = c("%Y-%m-%d"))

      assertNumber(params$height)
      assertNumber(params$weight)

      params$race <- tolower(params$race) %||% ""
      assertChoice(params$race, c("white", "afro-caribbean", "south-asian", "east-asian", "mixed"))

      params$smoking <- tolower(params$smoking) %||% ""
      assertChoice(params$smoking, c("yes", "no"))

      params$mother_pe <- tolower(params$mother_pe) %||% ""
      assertChoice(params$mother_pe, c("yes", "no"))

      params$conception <- tolower(params$conception) %||% ""
      assertChoice(params$conception, c("spontaneous", "ovulation drugs", "ivf"))

      # Medical History
      params$diabetes_type_i <- tolower(params$diabetes_type_i) %||% ""
      assertChoice(params$diabetes_type_i,  c("yes", "no"))

      params$diabetes_type_ii <- tolower(params$diabetes_type_ii) %||% ""
      assertChoice(params$diabetes_type_ii, c("yes", "no"))

      if ( params$diabetes_type_i == "yes" || params$diabetes_type_ii == "yes" ) {
        params$diabetes_drugs <- tolower(params$diabetes_drugs) %||% ""
        assertChoice(params$diabetes_drugs, c("diet", "insulin", "insulin+metformin", "metformin"))
      }

      params$sle <- tolower(params$sle) %||% ""
      assertChoice(params$sle, c("yes", "no"))

      params$aps <- tolower(params$aps) %||% ""
      assertChoice(params$aps, c("yes", "no"))

      if (params$aps == "yes" || params$sle == "yes") stop("SLE and APS not supported")

      params$chronic_hypertension <- tolower(params$chronic_hypertension) %||% ""
      assertChoice(params$chronic_hypertension, c("yes", "no"))

      # Obstetric History
      params$previous <- tolower(params$previous) %||% ""
      assertChoice(params$previous, c("yes", "no"))

      if ( params$previous == "yes") {
        params$previous_pe <- tolower(params$previous_pe) %||% ""
        assertChoice(params$previous_pe, c("yes", "no"))

        if ( is.null(params[["previous_delivered_at"]]) ) stop("Please provide timimng of last delivery: 'previous_delivered_at'")
        if ( !is.character(params[["previous_delivered_at"]]) ) warning("Note: Converting 'previous_delivered_at' to date object")
        params[["previous_delivered_at"]] <- as.Date(params[["previous_delivered_at"]], tryFormats = c("%Y-%m-%d"))

        assertNumber(params$previous_ga_weeks)
        assertNumber(params$previous_ga_days)
      }

      # Biophysical Measurements
      assertNumber(params$map,  na.ok = TRUE)
      assertNumber(params$mom_map,  na.ok = TRUE)

      assertNumber(params$utpi, na.ok = TRUE)
      assertNumber(params$mom_utpi, na.ok = TRUE)

      if ( !is.na(params$map) || !is.na(params$utpi) ) {
        if ( is.null(params[["biophysical_at"]]) ) stop("Please provide timimng of biophysical examination: 'biophysical_at'")
        if ( !is.character(params[["biophysical_at"]]) ) warning("Note: Converting 'biophysical_at' to date object")
        params[["biophysical_at"]] <- as.Date(params[["biophysical_at"]], tryFormats = c("%Y-%m-%d"))
      } else {
        params[["biophysical_at"]] <- NULL
      }

      # Biochemical Measurements
      assertNumber(params$plgf, na.ok = TRUE)
      assertNumber(params$mom_plgf, na.ok = TRUE)

      if ( !is.na(params$plgf) ) {

        params$plgf_machine <- tolower(params$plgf_machine) %||% ""
        assertChoice(params$plgf_machine, c("delfia", "kryptor", "roche"))

        if ( is.null(params[["biochemical_at"]]) ) stop("Please provide timimng of biochemical examination: 'biochemical_at'")
        if ( !is.character(params[["biochemical_at"]]) ) warning("Note: Converting 'biochemical_at' to date object")
        params[["biochemical_at"]] <- as.Date(params[["biochemical_at"]], tryFormats = c("%Y-%m-%d"))

      } else if(!is.na(params$mom_plgf)) {

        if ( is.null(params[["biochemical_at"]]) ) stop("Please provide timimng of biochemical examination: 'biochemical_at'")
        if ( !is.character(params[["biochemical_at"]]) ) warning("Note: Converting 'biochemical_at' to date object")
        params[["biochemical_at"]] <- as.Date(params[["biochemical_at"]], tryFormats = c("%Y-%m-%d"))

      } else {

        params[["biochemical_at"]] <- NULL

      }

      # Get relative gestational ages
      params$pregnancy_start_at <- params$ga_at - params$ga*7
      params$pregnancy_term_at  <- params$pregnancy_start_at + 40*7

      if (params$previous == "yes") {
        params$previous_ga       <- params$previous_ga_weeks + params$previous_ga_days/7
        params$previous_interval <- as.numeric(difftime(params$pregnancy_start_at, params$previous_delivered_at, units = "days")) / 365.25
      }

      if ( !is.null(params$biochemical_at) ) {
        params$biochemical_ga <- as.numeric(difftime(params$biochemical_at, params$pregnancy_start_at, units = "weeks"))
      }

      if ( !is.null(params$biophysical_at) ) {
        params$biophysical_ga <- as.numeric(difftime(params$biophysical_at, params$pregnancy_start_at, units = "weeks"))
      }

      # Maternal age at estimated date of delivery (years)
      # https://doi.org/10.1016/j.ajog.2019.11.1247
      params$age <- as.numeric(difftime(params$pregnancy_term_at, params$date_of_birth, units = "days")) / 365.25

      # Assignments
      self$params     <- params
      self$risk_model <- risk_model

      # Validate according to specific risk model
      self$risk_model$validate(self)

      # Estimate expected map
      self$params[["expected_map"]] <- self$risk_model$get_expected_map(self)

      # Estimate expected utpi
      self$params[["expected_utpi"]] <- self$risk_model$get_expected_utpi(self)

      # Estimate expected plgf
      self$params[["expected_plgf"]] <- self$risk_model$get_expected_plgf(self)

      # Estimate mom_map
      if ( is.na(self$params[["mom_map"]]) ) {
        self$params[["mom_map"]] <- self$get("map") / self$get("expected_map")
      }

      # Estimate mom_utpi
      if ( is.na(self$params[["mom_utpi"]]) ) {
        self$params[["mom_utpi"]] <- self$get("utpi") / self$get("expected_utpi")
      }

      # Estimate mom_plgf
      if ( is.na(self$params[["mom_plgf"]]) ) {
        self$params[["mom_plgf"]] <- self$get("plgf") / self$get("expected_plgf")
      } else {
        if ( risk_model$model_name == "RiskModelFMFM2025" ) {
          # I am unable to validate provided moms for plgf through the online calculator
          warning("Only raw plgf values are allowed for `RiskModelFMFM2025` - mom plgf values will be ignored!")
          self$params[["mom_plgf"]] <- NA
          self$params[["biochemical_ga"]] <- NA
        }
      }

      # Estimate prior risk
      if ( is.null(self$params[["prior_risk"]]) || is.na(self$params[["prior_risk"]]) ) {
        self$params[["prior_risk"]] <- self$risk_model$get_prior_risk(self, pnorm = TRUE)
        self$params[["prior_risk_text"]] <- risk_to_text(self$params[["prior_risk"]])
      }

      # Estimate risk
      if ( is.null(self$params[["risk"]]) || is.na(self$params[["risk"]]) ) {
        self$params[["risk"]] <- self$risk_model$get_risk(self)
        self$params[["risk_text"]] <- risk_to_text(self$params[["risk"]])
      }

    },

    #' @description Get a parameter value, optionally truncated.
    #' @param param Character string of the parameter name.
    #' @param truncate_for Character string indicating the truncation context (e.g., "risk").
    #' @return The numeric or character value of the parameter.
    get = function(param, truncate_for = "") {

      if (!param %in% names(self$params)) {
        return(NA_real_)
      }
      if (truncate_for == "") {

        return(self$get_raw(param))

      } else {

        return(
          self$risk_model$truncate(
            pregnancy = self,
            param = param,
            truncate_for = truncate_for
            )
          )

      }
    },

    #' @description Get raw parameter value.
    #' @param param Character string of the parameter name.
    get_raw = function(param) self$params[[param]],

    #' @description Export pregnancy data as a data table.
    #' @return A \code{data.table} object.
    as_df = function() as.data.table(self$params)

  )
)

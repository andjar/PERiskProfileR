#' RiskModel Base Class
#'
#' @description
#' An abstract R6 class that serves as the foundation for specific
#' preeclampsia risk models. It defines the standard
#' interface and shared fields required for risk calculations.
#'
#' @field model_name Character. The name of the specific risk model
#'   (defaults to "RiskModelTemplate").
#' @field G Numeric. The gestational age at which risk is being calculated
#'   (default is 37).
#' @field risk_model Placeholder for the specific model implementation
#'   logic.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(G = 37)}}{Constructor to initialize the model
#'     with a specific gestational age.}
#'   \item{\code{truncate(pregnancy, param, truncate_for)}}{Abstract method
#'     to handle value truncation for specific parameters.}
#'   \item{\code{get_ga_from_crl(crl)}}{Abstract method to calculate
#'     gestational age based on Crown-Rump Length.}
#'   \item{\code{validate(crl)}}{Abstract method to validate clinical
#'     inputs such as CRL.}
#'   \item{\code{get_expected_map(pregnancy)}}{Abstract method to calculate
#'     the expected Mean Arterial Pressure.}
#'   \item{\code{get_expected_utpi(pregnancy)}}{Abstract method to calculate
#'     the expected Uterine Artery Pulsatility Index.}
#'   \item{\code{get_expected_plgf(pregnancy)}}{Abstract method to calculate
#'     the expected Placental Growth Factor.}
#'   \item{\code{get_prior_risk(pregnancy, g = 37, pnorm = FALSE)}}{Abstract
#'     method to calculate the baseline risk before biomarkers.}
#'   \item{\code{get_risk(pregnancy, G = 37)}}{Abstract method to calculate
#'     the final posterior risk of preeclampsia.}
#' }
#'
#' @export
RiskModel <- R6::R6Class(
  classname = "RiskModel",
  public = list(
    model_name = "RiskModelTemplate",
    G = NULL,
    risk_model = NULL,

    #' @description Create a new RiskModel object.
    #' @param G Gestational age.
    initialize = function(G = 37) {
      self$G = G
    },

    #' @description Truncate clinical values.
    #' @param pregnancy A \code{Pregnancy} object.
    #' @param param The parameter name to truncate.
    #' @param truncate_for The context for truncation.
    truncate = function(pregnancy, param, truncate_for) {
      stop("Must implement truncate() in subclass")
    },

    #' @description Get GA from CRL.
    #' @param crl Crown-Rump Length (mm).
    get_ga_from_crl = function(crl) {
      stop("Must implement get_ga_from_crl() in subclass")
    },

    #' @description Validate CRL.
    #' @param crl Crown-Rump Length (mm).
    validate = function(crl) {
      stop("Must implement validate() in subclass")
    },

    #' @description Get expected MAP MoM.
    #' @param pregnancy A \code{Pregnancy} object.
    get_expected_map = function(pregnancy) {
      stop("Must implement get_expected_map() in subclass")
    },

    #' @description Get expected UtPI MoM.
    #' @param pregnancy A \code{Pregnancy} object.
    get_expected_utpi = function(pregnancy) {
      stop("Must implement get_expected_utpi() in subclass")
    },

    #' @description Get expected PlGF MoM.
    #' @param pregnancy A \code{Pregnancy} object.
    get_expected_plgf = function(pregnancy) {
      stop("Must implement get_expected_plgf() in subclass")
    },

    #' @description Get prior risk.
    #' @param pregnancy A \code{Pregnancy} object.
    #' @param g Gestational age (default 37).
    #' @param pnorm Logical; whether to return the normal distribution probability.
    get_prior_risk = function(pregnancy, g = 37, pnorm = FALSE) {
      stop("Must implement get_prior_risk() in subclass")
    },

    #' @description Calculate final risk.
    #' @param pregnancy A \code{Pregnancy} object.
    #' @param G Gestational age (default 37).
    get_risk = function(pregnancy, G = 37) {
      stop("Must implement get_risk() in subclass")
    }
  )
)

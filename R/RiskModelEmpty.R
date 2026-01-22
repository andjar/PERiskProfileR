#' RiskModelEmpty Class
#'
#' @description
#' A subclass of \code{\link{RiskModel}} that implements a placeholder or
#' "empty" risk calculation. While it provides the required interface,
#' it returns \code{NA} for biomarker and risk calculations. This is primarily
#' used as a baseline or for testing purposes.
#'
#' @super RiskModel
#'
#' @field model_name Character. Always set to "RiskModelEmpty".
#' @field G Numeric. The gestational age at which risk is calculated.
#' @field risk_model Placeholder for consistency with the base class.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(G = 37)}}{Constructor to initialize the model
#'     with a specific gestational age.}
#'   \item{\code{get_ga_from_crl(pregnancy = NA, crl = NA)}}{Calculates
#'     gestational age (GA) from Crown-Rump Length (CRL) using a standard
#'     formula.}
#'   \item{\code{truncate(pregnancy, param, truncate_for)}}{A placeholder
#'     method that performs no truncation.}
#'   \item{\code{validate(crl)}}{A placeholder method for CRL validation.}
#'   \item{\code{get_expected_map(pregnancy)}}{Returns \code{NA_real_}.}
#'   \item{\code{get_expected_plgf(pregnancy)}}{Returns \code{NA_real_}.}
#'   \item{\code{get_expected_utpi(pregnancy)}}{Returns \code{NA_real_}.}
#'   \item{\code{get_prior_risk(pregnancy, g = 37, pnorm = FALSE)}}{Returns \code{NA_real_}.}
#'   \item{\code{get_risk(pregnancy, G = 37)}}{Returns \code{NA_real_}.}
#' }
#'
#' @export
RiskModelEmpty <- R6::R6Class(
  classname = "RiskModelEmpty",
  inherit = RiskModel,
  public = list(
    model_name = "RiskModelEmpty",
    G = NULL,
    risk_model = NULL,

    #' @description Create a new RiskModelEmpty object.
    #' @param G Gestational age (default 37).
    initialize = function(G = 37) {
      self$G = G
    },

    #' @description Calculate GA from CRL.
    #' @param pregnancy Optional \code{Pregnancy} object to extract CRL from.
    #' @param crl Numeric. Crown-Rump Length (mm).
    #' @return Numeric gestational age.
    get_ga_from_crl = function(pregnancy = NA, crl = NA) {
      if ( is.na(crl) ) {
        if ( is.na(pregnancy) ) {
          stop("CRL not provided.")
        } else if ( is.na(pregnancy$get("crl")) ) {
          stop("CRL not provided.")
        } else {
          crl <- pregnancy$get("crl")
        }
      }

      ga <- 23.73 + 8.052 * sqrt(1.037*crl)
      return(ga)
    },

    #' @description No-op truncation.
    #' @param pregnancy A \code{Pregnancy} object.
    #' @param param Parameter name.
    #' @param truncate_for Truncation context.
    truncate = function(pregnancy, param, truncate_for) {},

    #' @description No-op validation.
    #' @param crl CRL value.
    validate = function(crl) {},

    #' @description Get expected MAP (placeholder).
    #' @param pregnancy A \code{Pregnancy} object.
    get_expected_map = function(pregnancy) {
      return(NA_real_)
    },

    #' @description Get expected PlGF (placeholder).
    #' @param pregnancy A \code{Pregnancy} object.
    get_expected_plgf = function(pregnancy) {
      return(NA_real_)
    },

    #' @description Get expected UtPI (placeholder).
    #' @param pregnancy A \code{Pregnancy} object.
    get_expected_utpi = function(pregnancy) {
      return(NA_real_)
    },

    #' @description Get prior risk (placeholder).
    #' @param pregnancy A \code{Pregnancy} object.
    #' @param g Gestational age.
    #' @param pnorm Logical.
    get_prior_risk = function(pregnancy, g = 37, pnorm = FALSE) {
      return(NA_real_)
    },

    #' @description Get final risk (placeholder).
    #' @param pregnancy A \code{Pregnancy} object.
    #' @param G Gestational age.
    get_risk = function(pregnancy, G = 37) {
      return(NA_real_)
    }
  )
)

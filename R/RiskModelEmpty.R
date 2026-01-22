RiskModelEmpty <- R6::R6Class(
  classname = "RiskModelEmpty",
  inherit = RiskModel,
  public = list(
    model_name = "RiskModelEmpty",
    G = NULL,
    risk_model = NULL,
    initialize = function(G = 37) {
      self$G = G
    },
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

      ga <- 23.53 + 8.052 * sqrt(1.037*crl)
      return(ga)
    },
    truncate = function(pregnancy, param, truncate_for) {},
    validate = function(crl) {},
    get_expected_map = function(pregnancy) {
      return(NA_real_)
    },
    get_expected_plgf = function(pregnancy) {
      return(NA_real_)
    },
    get_expected_utpi = function(pregnancy) {
      return(NA_real_)
    },
    get_prior_risk = function(pregnancy, g = 37, pnorm = FALSE) {
      return(NA_real_)
    },
    get_risk = function(pregnancy, G = 37) {
      return(NA_real_)
    }
  )
)

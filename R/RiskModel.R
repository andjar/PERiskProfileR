RiskModel <- R6::R6Class(
  classname = "RiskModel",
  public = list(
    model_name = "RiskModelTemplate",
    G = NULL,
    risk_model = NULL,
    initialize = function(G = 37) {
      self$G = G
    },
    truncate = function(pregnancy, param, truncate_for) {
      stop("Must implement truncate() in subclass")
    },
    get_ga_from_crl = function(crl) {
      stop("Must implement get_ga_from_crl() in subclass")
    },
    validate = function(crl) {
      stop("Must implement validate() in subclass")
    },
    get_expected_map = function(pregnancy) {
      stop("Must implement get_expected_map() in subclass")
    },
    get_expected_utpi = function(pregnancy) {
      stop("Must implement get_expected_utpi() in subclass")
    },
    get_expected_plgf = function(pregnancy) {
      stop("Must implement get_expected_plgf() in subclass")
    },
    get_prior_risk = function(pregnancy, g = 37, pnorm = FALSE) {
      stop("Must implement get_prior_risk() in subclass")
    },
    get_risk = function(pregnancy, G = 37) {
      stop("Must implement get_risk() in subclass")
    }
  )
)

#' FMF 2025 Risk Model Class
#'
#' @description
#' An R6 class implementing the updated 2025 competitive risk model for
#' preeclampsia. This model expands upon the 2023 version by supporting
#' earlier screening (from 8 weeks) and incorporating refined biomarker
#' coefficients.
#'
#' @inherit RiskModel
#'
#' @details
#' This implementation is based on FMF Models main/0.0.1 and supports
#' biochemical and biophysical screening between 56 and 99 days of
#' gestation (8+0 to 14+1 weeks).
#'
#' \strong{Key differences from the 2023 model:}
#' \itemize{
#'   \item \strong{Earlier Screening:} Valid GA range is 56-99 days (vs 77-99 days).
#'   \item \strong{Refined PlGF Logic:} Includes a cubic term for GA and
#'     incorporates maternal height into the expected PlGF calculation.
#'   \item \strong{Early PlGF Handling:} If PlGF is measured before 12 weeks
#'     and exceeds a specific threshold, it is treated as \code{NA} to
#'     prevent over-estimation of risk.
#'   \item \strong{MAP Required:} This model requires MAP measurements (unlike 2023).
#'   \item \strong{Separate Truncations:} Uses separate truncation limits for
#'     each biomarker (mom_map, mom_utpi, mom_plgf).
#' }
#'
#' \strong{Note:} Only raw PlGF values are accepted for this model. Pre-calculated
#' MoM values for PlGF will be ignored with a warning.
#'
#' @field truncations A nested list of clinical limits. Note that the 2025
#'   model uses separate truncation lists for each biomarker (MAP, UtPI, PlGF).
#'
#' @examples
#' \dontrun{
#' # Create a 2025 model for risk before 37 weeks
#' model <- RiskModelFMFM2025$new(G = 37)
#'
#' # Use with calculate_risk for early screening
#' results <- calculate_risk(df, model = "FMF2025", G = 37)
#' }
#'
#' @seealso \code{\link{RiskModel}} for the base class,
#'   \code{\link{RiskModelFMFM2023}} for the 2023 model,
#'   \code{\link{Pregnancy}} for the Pregnancy class,
#'   \code{\link{calculate_risk}} for batch processing
#'
#' @export
RiskModelFMFM2025 <- R6::R6Class(
  classname = "RiskModelFMFM2025",
  inherit = RiskModel,
  public = list(
    model_name = "RiskModelFMFM2025",
    G = NULL,
    risk_model = NULL,
    truncations = list(
      "mom_map"    = list(
        weight = c(34, 133),
        height = c(127, 198),
        age    = c(15, 45),
        previous_interval = c(0.25, 20)
      ),
      "mom_utpi"    = list(
        weight = c(34, 133),
        height = c(127, 198),
        age    = c(15, 45),
        previous_interval = c(0.25, 20)
      ),
      "mom_plgf"    = list(
        weight = c(34, 164),
        height = c(127, 198),
        age    = c(15, 45),
        previous_interval = c(0.25, 20)
      ),
      "prior_risk" = list(
        weight = c(34, 190),
        height = c(127, 198),
        age    = c(12, 55),
        previous_ga = c(24, 42),
        previous_interval = c(0.25, 15)
      )
      ,
      "risk" = list(
        mom_map  = c(10^-0.12240759, 10^0.12240759),
        mom_utpi = c(10^-0.42161519, 10^0.42161519),
        mom_plgf = c(10^-0.56550992, 10^0.56550992)
      )
      # Source: https://doi.org/10.1016/j.ajog.2019.11.1247
    ),

    #' @description Inherited from RiskModel.
    #' @param G Target gestational age (default 37).
    initialize = function(G = 37) {
      self$G = G
    },

    #' @description Truncate clinical values based on model limits.
    #' @param pregnancy A \code{Pregnancy} object.
    #' @param param Parameter name.
    #' @param truncate_for Context ("mom", "prior_risk", or "risk").
    truncate = function(pregnancy, param, truncate_for) {
      limits <- self$truncations[[truncate_for]][[param]]
      value <- pregnancy$get_raw(param)
      return(pmax(limits[1], pmin(value, limits[2])))
    },

    #' @description Specific validation for the 2025 model.
    #' @inheritParams RiskModel
    validate = function(pregnancy) {
      assertNumber(pregnancy$get("ga")*7, lower = 56, upper = 99, .var.name = "Gestational age in days")
      assertNumber(pregnancy$get("biophysical_ga")*7, na.ok = TRUE, null.ok = TRUE, lower = 56, upper = 99, .var.name = "Gestational age at biophysical examination in days")
      assertNumber(pregnancy$get("biochemical_ga")*7, na.ok = TRUE, null.ok = TRUE, lower = 56, upper = 99, .var.name = "Gestational age at biochemical examination in days")

      assertNumber(pregnancy$get("map"),  na.ok = FALSE, .var.name = "Provided MAP measurements")
    },

    #' @description Inherits CRL conversion from \code{RiskModel}.
    #' @inheritParams RiskModel
    get_ga_from_crl = function(crl = NA) {
      if ( is.na(crl) ) {
        stop("CRL not provided.")
      }
      ga <- floor(23.73 + 8.052 * sqrt(1.037*crl))/7
      return(ga)
    },

    #' @description Refined MAP calculation for 2025.
    #' @inheritParams RiskModel
    get_expected_map = function(pregnancy) {
      intercept    <-                      1.936400000
      beta_ga      <-                      0.000428017
      beta_ga2     <-                     -0.000028811
      beta_weight  <-                      0.001205300
      beta_weight2 <-                     -0.000009280
      beta_height  <-                     -0.000181570
      beta_afro_caribbean <-              -0.003930000
      beta_smoking <-                     -0.008640000
      beta_chronic_hypertension <-         0.053630000
      beta_chronic_hypertension_weight <- -0.000239750
      beta_DM <-                           0.004370000
      beta_family_PE <-                    0.006240000

      mom <- intercept +

        beta_ga  * (pregnancy$get("biophysical_ga")*7-77) +
        beta_ga2 * (pregnancy$get("biophysical_ga")*7-77)^2 +
        beta_weight  * (pregnancy$get("weight", truncate_for = "mom_map") - 69) +
        beta_weight2 * (pregnancy$get("weight", truncate_for = "mom_map") - 69)^2 +
        beta_height  * (pregnancy$get("height", truncate_for = "mom_map") - 164) +

        ifelse(pregnancy$get("race") == "afro-caribbean", beta_afro_caribbean, 0) +

        ifelse(pregnancy$get("smoking") == "yes", beta_smoking, 0) +
        ifelse(pregnancy$get("chronic_hypertension") == "yes", beta_chronic_hypertension, 0) +
        ifelse(pregnancy$get("chronic_hypertension") == "yes",
               beta_chronic_hypertension_weight * (pregnancy$get("weight", truncate_for = "mom_map") - 69),
               0) +
        ifelse(pregnancy$get("diabetes_type_i") == "yes" || pregnancy$get("diabetes_type_ii") == "yes", beta_DM, 0) +
        ifelse(pregnancy$get("mother_pe") == "yes", beta_family_PE, 0)

      if (pregnancy$get("previous") == "yes") {
        if (pregnancy$get("previous_pe") == "yes") {
          intercept_parous <- 0.008570000

          mom <- mom +
            intercept_parous

        } else {
          intercept_parous <- -0.006630000
          beta_interval    <-  0.000826390

          mom <- mom +
            intercept_parous +
            beta_interval * (pregnancy$get("previous_interval", truncate_for = "mom_map") - 2)

        }
      }

      return(10^mom)
    },

    #' @description Refined PlGF calculation for 2025.
    #' @inheritParams RiskModel
    get_expected_plgf = function(pregnancy) {
      intercept           <-  0
      intercept_delfia    <-  1.346177
      intercept_cobas     <-  1.563884
      intercept_kryptor   <-  1.362244
      beta_ga             <-  0.008821
      beta_ga2            <-  0.000037
      beta_ga3            <-  0.000012
      beta_weight         <- -0.000945
      beta_height         <- -0.001824
      beta_age            <-  0.002333
      beta_afro_caribbean <-  0.170878
      beta_south_asian    <-  0.071848
      beta_east_asian     <-  0.032084
      beta_mixed          <-  0.057355
      beta_smoking        <-  0.160358
      beta_DM_1           <- -0.040639
      beta_DM_2           <- -0.033537
      beta_in_vitro       <- -0.020541
      beta_parous_PE      <- -0.018429
      beta_parous_no_PE   <-  0.015207

      mom <- intercept +

        # Manufacturer
        # Intercept: DELFIA Xpress
        ifelse(pregnancy$get("plgf_machine") == "delfia", intercept_delfia,  0) +

        # Intercept: BRAHMS KRYPTOR
        ifelse(pregnancy$get("plgf_machine") == "kryptor", intercept_kryptor, 0) +

        # Intercept: Cobas e411 / Roche
        ifelse(pregnancy$get("plgf_machine") == "roche", intercept_cobas,   0) +

        # GA and weight
        # Gestational age in days – 77
        beta_ga  * (pregnancy$get("biochemical_ga") * 7 - 77) +

        # (Gestational age in days – 77)^2
        beta_ga2 * (pregnancy$get("biochemical_ga") * 7 - 77)^2 +

        # (Gestational age in days – 77)^3
        beta_ga3 * (pregnancy$get("biochemical_ga") * 7 - 77)^3 +

        # Weight in kg - 69
        beta_weight  * (pregnancy$get("weight", truncate_for = "mom_plgf") - 69) +

        # (Height in cm - 164)
        beta_height * (pregnancy$get("height", truncate_for = "mom_plgf") - 164) +

        # Maternal age in years - 35
        beta_age * (pregnancy$get("age", truncate_for = "mom_plgf") - 35) +

        # Ethnicity
        # Racial origin: Afro-Caribbean
        ifelse(pregnancy$get("race") == "afro-caribbean", beta_afro_caribbean, 0) +

        # Racial origin: South Asian
        ifelse(pregnancy$get("race") == "south-asian", beta_south_asian, 0) +

        # Racial origin: East Asian
        ifelse(pregnancy$get("race") == "east-asian", beta_east_asian, 0) +

        # Racial origin: Mixed
        ifelse(pregnancy$get("race") == "mixed", beta_mixed, 0) +

        # Smoking, diabetes, conception
        # Smoker
        ifelse(pregnancy$get("smoking") == "yes", beta_smoking, 0) +

        # Medical history of diabetes mellitus Type 1
        ifelse(pregnancy$get("diabetes_type_i") == "yes", beta_DM_1, 0) +

        # Medical history of diabetes mellitus Type 2 treated with insulin
        ifelse(pregnancy$get("diabetes_type_ii") == "yes", beta_DM_2, 0) +

        # In-vitro fertilization
        ifelse(pregnancy$get("conception") == "ivf", beta_in_vitro, 0) +

        # Parous with history of pre-eclampsia
        ifelse(pregnancy$get("previous") == "yes" && pregnancy$get("previous_pe") == "yes", beta_parous_PE, 0) +

        # Parous with no history of pre-eclampsia
        ifelse(pregnancy$get("previous") == "yes" && pregnancy$get("previous_pe") == "no", beta_parous_no_PE, 0)

      return(10^mom)
    },

    #' @description Refined UtPI calculation for 2025.
    #' @inheritParams RiskModel
    get_expected_utpi = function(pregnancy) {
      intercept           <-  0.264570000
      beta_ga             <- -0.004838365
      beta_weight         <- -0.000874430
      beta_weight2        <-  0.000007330
      beta_age            <- -0.000641750
      beta_afro_caribbean <-  0.021620000
      beta_east_asian     <-  0.007630000
      beta_mixed          <-  0.011990000
      beta_DM1            <- -0.027490000

      mom <- intercept +

        # GA, maternal age and weight
        # Gestational age in days – 77
        beta_ga * (floor(pregnancy$get("biophysical_ga")*7) - 77) +
        beta_weight  * (pregnancy$get("weight", truncate_for = "mom_utpi") - 69) +
        beta_weight2 * (pregnancy$get("weight", truncate_for = "mom_utpi") - 69)^2 +
        beta_age * (pregnancy$get("age", truncate_for = "mom_utpi") - 35) +

        # Ethnicity
        # Racial origin: Afro-Caribbean
        ifelse(pregnancy$get("race") == "afro-caribbean", beta_afro_caribbean, 0) +

        # Racial origin: East Asian
        ifelse(pregnancy$get("race") == "east-asian", beta_east_asian, 0) +

        # Racial origin: Mixed
        ifelse(pregnancy$get("race") == "mixed", beta_mixed, 0) +

        # Diabetes
        # Diabetes type 1
        ifelse(pregnancy$get("diabetes_type_i") == "yes", beta_DM1, 0)

      if (pregnancy$get("previous") == "yes") {
        if (pregnancy$get("previous_pe") == "yes") {
          intercept_parous <- 0.009650000
          mom <- mom + intercept_parous
        } else {
          intercept_parous <- -0.002950000
          mom <- mom + intercept_parous
        }
      }

      return(10^mom)
    },

    #' @description Refined prior risk calculation for 2025.
    #' @inheritParams RiskModel
    get_prior_risk = function(pregnancy, g = 37, pnorm = FALSE) {

      age    <- pmax(12,  pmin(pregnancy$get("age"), 55))
      height <- pmax(127, pmin(pregnancy$get("height"), 198))
      weight <- pmax(34,  pmin(pregnancy$get("weight"), 190))


      intercept <- 54.3637
      mu <- intercept


      if (age > 35) {
        mu <- mu + (age - 35) * -0.206886
      }

      mu <- mu + (height - 164) * 0.11711

      mu <- mu + (ifelse(pregnancy$get("race") == "afro-caribbean", 1, 0) * -2.6786)
      mu <- mu + (ifelse(pregnancy$get("race") == "south-asian", 1, 0) * -1.129)
      mu <- mu + (ifelse(pregnancy$get("chronic_hypertension") == "yes", 1, 0) * -7.2897)
      mu <- mu + (ifelse(pregnancy$get("conception") == "ivf", 1, 0) * -1.6327)

      if ( pregnancy$get("previous") == "yes" ) {
        if ( pregnancy$get("previous_pe") == "yes" ) {
          intercept_parity <- -8.1667
          beta_previous_ga <- 0.0271988

          mu <- mu +
            intercept_parity +
            beta_previous_ga * (pregnancy$get("previous_ga", truncate_for = "prior_risk") - 24)^2

        } else {
          intercept_parity <- -4.335
          beta_interval    <- -4.15137651
          beta_interval_05 <-  9.21473572
          beta_previous_ga <-  0.01549673

          mu <- mu +
            intercept_parity +
            beta_interval    * (pregnancy$get("previous_interval", truncate_for = "prior_risk")^-1) +
            beta_interval_05 * (pregnancy$get("previous_interval", truncate_for = "prior_risk")^-0.5) +
            beta_previous_ga * (pregnancy$get("previous_ga", truncate_for = "prior_risk") - 24)^2
        }
      }

      if ( pregnancy$get("chronic_hypertension") == "no" ) {
        beta_weight               <- -0.0694096
        beta_family_PE            <- -1.7154
        beta_DM                   <- -3.3899

        mu <- mu +
          beta_weight * (pregnancy$get("weight", truncate_for = "prior_risk") - 69) +
          ifelse(pregnancy$get("mother_pe") == "yes", beta_family_PE, 0) +
          ifelse(pregnancy$get("diabetes_type_i") == "yes" || pregnancy$get("diabetes_type_ii") == "yes", beta_DM, 0)
      }

      sigma <- 6.8833

      if (pnorm) {
        return(pnorm(g, mean = mu, sd = sigma))
      } else {
        return(dnorm(g, mean = mu, sd = sigma))
      }
    },

    #' @description Refined risk calculation for 2025.
    #' @inheritParams RiskModel
    get_risk = function(pregnancy, G = 37) {

      get_mus <- function(mom_MAP, mom_PI, mom_PlGF, x) {
        b0_MAP <-  0.088997
        b1_MAP <- -0.0016711

        b0_PI <-  0.5861
        b1_PI <- -0.014233

        b0_PlGF <- -0.92352
        b1_PlGF <- 0.021584

        mu_MAP  <- ifelse(x < -b0_MAP/b1_MAP, b0_MAP + b1_MAP * x, 0)
        mu_PI   <- ifelse(x < -b0_PI/b1_PI, b0_PI + b1_PI * x, 0)
        mu_PlGF <- ifelse(x < -b0_PlGF/b1_PlGF, b0_PlGF + b1_PlGF * x, 0)

        return(
          c(mu_MAP, mu_PI, mu_PlGF)[c(!is.na(mom_MAP), !is.na(mom_PI), !is.na(mom_PlGF))]
        )
      }

      get_covariance_matrix <- function(has_map = TRUE, has_utpi = TRUE, has_plgf = TRUE) {
        # https://www.nejm.org/doi/suppl/10.1056/NEJMoa1704559/suppl_file/nejmoa1704559_appendix.pdf

        # MAP, PI, PlGF
        covariance_matrix <- matrix(
          c(
            0.00141396, -0.0002726, -0.0001907,
            -0.0002726, 0.01630906, -0.0034539,
            -0.0001907, -0.0034539, 0.03147225),
          3, 3)

        ind_to_return <- c(1,2,3)[c(has_map, has_utpi, has_plgf)]

        return(covariance_matrix[ind_to_return, ind_to_return])
      }

      get_p_MAP_PI_PlGF <- function(mom_MAP, mom_PI, mom_PlGF, g) {

        covariance_matrix <- get_covariance_matrix(
          has_map  = !is.na(mom_MAP),
          has_utpi = !is.na(mom_PI),
          has_plgf = !is.na(mom_PlGF)
        )

        vals_at_point <- c(
          log10(mom_MAP), log10(mom_PI), log10(mom_PlGF)
        )[c(!is.na(mom_MAP), !is.na(mom_PI), !is.na(mom_PlGF))]

        sapply(g, function(x) {
          emdbook::dmvnorm(
            vals_at_point,
            mu = get_mus(mom_MAP, mom_PI, mom_PlGF, x),
            Sigma = covariance_matrix
          )
        })

      }

      get_p_prior <- function(g, pregnancy, mom_MAP = NA, mom_PI = NA, mom_PlGF = NA) {
        get_p_MAP_PI_PlGF(mom_MAP = mom_MAP, mom_PI = mom_PI, mom_PlGF = mom_PlGF, g = g) * self$get_prior_risk(pregnancy, g = g)
      }

      mom_MAP  <- pregnancy$get("mom_map",  truncate_for = "risk")
      mom_PI   <- pregnancy$get("mom_utpi", truncate_for = "risk")
      mom_PlGF <- pregnancy$get("mom_plgf", truncate_for = "risk")

      if ( !is.na(pregnancy$get("biochemical_ga")) && pregnancy$get("biochemical_ga") < 12 ) {
        if ( !is.na(mom_PI) & mom_PlGF > 10^-0.1 ) {
          mom_PlGF <- NA
        }
      }





      risk <- integrate(get_p_prior,
                        lower = 24, upper = G,
                        pregnancy = pregnancy,
                        mom_MAP = mom_MAP, mom_PI = mom_PI, mom_PlGF = mom_PlGF
                        )$value / integrate(
          get_p_prior, lower = 24, upper = 100,
          pregnancy = pregnancy,
          mom_MAP = mom_MAP, mom_PI = mom_PI, mom_PlGF = mom_PlGF
          )$value
      return(risk)
    }
  )
)

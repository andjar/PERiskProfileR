#' FMF 2023 Risk Model Class
#'
#' @description
#' An R6 class implementing the competitive risk model for preeclampsia
#' developed by the Fetal Medicine Foundation (FMF). This model utilizes
#' maternal factors, biophysical markers (MAP, UtPI), and biochemical
#' markers (PlGF) to estimate the probability of delivery with preeclampsia
#' before a specified gestational age.
#'
#' @details
#' The model is based on the algorithm described in:
#' \href{https://doi.org/10.1016/j.ajog.2019.11.1247}{Tan et al. (2019)}.
#' It calculates risk by combining a prior distribution (based on maternal
#' characteristics) with the likelihood of biomarker Multiples of the
#' Median (MoMs).
#'
#' @super RiskModel
#'
#' @field model_name Character. Always "RiskModelFMFM2023".
#' @field truncations A nested list containing the lower and upper limits
#'   for clinical parameters (weight, height, age, etc.) used in different
#'   stages of calculation ("mom", "prior_risk", "risk").
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(G = 37)}}{
#'     Constructor. Sets the target gestational age for risk assessment.
#'   }
#'   \item{\code{truncate(pregnancy, param, truncate_for)}}{
#'     Limits the value of a parameter to the predefined range stored in
#'     \code{self$truncations}.
#'   }
#'   \item{\code{validate(pregnancy)}}{
#'     Ensures that the gestational age at the time of screening and biomarker
#'     collection falls within the valid model range (77 to 99 days).
#'   }
#'   \item{\code{get_ga_from_crl(crl = NA)}}{
#'     Calculates gestational age in weeks from Crown-Rump Length (mm)
#'     using the FMF standard formula.
#'   }
#'   \item{\code{get_expected_map(pregnancy)}}{
#'     Calculates the expected Mean Arterial Pressure (MAP) using a
#'     multiple regression model accounting for maternal traits.
#'   }
#'   \item{\code{get_expected_plgf(pregnancy)}}{
#'     Calculates the expected Placental Growth Factor (PlGF) based on
#'     maternal characteristics and the specific analyzer used (Delfia,
#'     Kryptor, or Roche).
#'   }
#'   \item{\code{get_expected_utpi(pregnancy)}}{
#'     Calculates the expected Uterine Artery Pulsatility Index (UtPI)
#'     MoM based on maternal characteristics.
#'   }
#'   \item{\code{get_prior_risk(pregnancy, g = 37, pnorm = FALSE)}}{
#'     Computes the prior risk distribution of delivering with preeclampsia
#'     based on the maternal "heart failure" model of PE.
#'   }
#'   \item{\code{get_risk(pregnancy, G = 37)}}{
#'     Calculates the final posterior risk. This method performs numerical
#'     integration of the joint probability of biomarkers and prior risk
#'     distributions.
#'   }
#' }
#'
#' @importFrom emdbook dmvnorm
#' @export
RiskModelFMFM2023 <- R6::R6Class(
  classname = "RiskModelFMFM2023",
  inherit = RiskModel,
  public = list(
    model_name = "RiskModelFMFM2023",
    G = NULL,
    risk_model = NULL,
    truncations = list(
      "mom"    = list(
        weight = c(0, 130),
        height = c(0, Inf),
        age    = c(0, Inf),
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

    #' @description Initialize the FMF 2023 model.
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

    #' @description Specific validation for the 2023 model (GA 77-99 days).
    #' @inheritParams RiskModel
    validate = function(pregnancy) {
      assertNumber(pregnancy$get("ga")*7, lower = 77, upper = 99, .var.name = "Gestational age in days")
      assertNumber(pregnancy$get("biophysical_ga")*7, na.ok = TRUE, null.ok = TRUE, lower = 77, upper = 99, .var.name = "Gestational age at biophysical examination in days")
      assertNumber(pregnancy$get("biochemical_ga")*7, na.ok = TRUE, null.ok = TRUE, lower = 77, upper = 99, .var.name = "Gestational age at biochemical examination in days")
    },

    #' @description Inherits CRL conversion from \code{RiskModel}.
    #' @inheritParams RiskModel
    get_ga_from_crl = function(crl = NA) {
      if ( is.na(pregnancy$get("crl")) || is.na(crl) ) {
        stop("CRL not provided.")
      }
      if ( is.na(crl) ) {
        crl <- pregnancy$get("crl")
      }
      ga <- 23.73 + 8.052 * sqrt(1.037*crl)
      return(ga)
    },

    #' @description Refined MAP calculation for 2023.
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
        beta_weight  * (pregnancy$get("weight", truncate_for = "mom") - 69) +
        beta_weight2 * (pregnancy$get("weight", truncate_for = "mom") - 69)^2 +
        beta_height  * (pregnancy$get("height", truncate_for = "mom") - 164) +

        ifelse(pregnancy$get("race") == "afro-caribbean", beta_afro_caribbean, 0) +

        ifelse(pregnancy$get("smoking") == "yes", beta_smoking, 0) +
        ifelse(pregnancy$get("chronic_hypertension") == "yes", beta_chronic_hypertension, 0) +
        ifelse(pregnancy$get("chronic_hypertension") == "yes",
               beta_chronic_hypertension_weight * (pregnancy$get("weight", truncate_for = "mom") - 69),
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
            beta_interval * (pregnancy$get("previous_interval", truncate_for = "mom") - 2)

        }
      }

      return(10^mom)
    },

    #' @description Refined PlGF calculation for 2023.
    #' @inheritParams RiskModel
    get_expected_plgf = function(pregnancy) {
      intercept           <-  0
      intercept_delfia    <-  1.332959332
      intercept_cobas     <-  1.542535524
      intercept_kryptor   <-  1.430615169
      beta_ga             <-  0.012263018
      beta_ga2            <-  0.000149743
      beta_weight         <- -0.001682761
      beta_weight2        <-  0.000008780
      beta_age            <-  0.002174191
      beta_afro_caribbean <-  0.193561059
      beta_south_asian    <-  0.072679108
      beta_east_asian     <-  0.034550109
      beta_mixed          <-  0.079010576
      beta_smoking        <-  0.160836176
      beta_DM_1           <- -0.029630891
      beta_DM_2           <- -0.039984195
      beta_in_vitro       <- -0.022250585
      beta_parous_no_PE   <-  0.020750050

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

        # Weight in kg - 69
        beta_weight  * (pregnancy$get("weight", truncate_for = "mom") - 69) +

        # (Weight in kg - 69)^2
        beta_weight2 * (pregnancy$get("weight", truncate_for = "mom") - 69)^2 +

        # Maternal age in years - 35
        beta_age * (pregnancy$get("age", truncate_for = "mom") - 35) +

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
        ifelse(pregnancy$get("diabetes_type_ii") == "yes" && pregnancy$get("diabetes_drugs") %in% c("insulin", "insulin+metformin"), beta_DM_2, 0) +

        # In-vitro fertilization
        ifelse(pregnancy$get("conception") == "ivf", beta_in_vitro, 0) +

        # Parous with no history of pre-eclampsia
        ifelse(pregnancy$get("previous") == "yes" && pregnancy$get("previous_pe") == "no", beta_parous_no_PE, 0)

      return(10^mom)
    },

    #' @description Refined UtPI calculation for 2023.
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
        beta_ga * (pregnancy$get("biophysical_ga")*7 - 77) +
        beta_weight  * (pregnancy$get("weight", truncate_for = "mom") - 69) +
        beta_weight2 * (pregnancy$get("weight", truncate_for = "mom") - 69)^2 +
        beta_age * (pregnancy$get("age", truncate_for = "mom") - 35) +

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

    #' @description Refined prior risk calculation for 2023.
    #' @inheritParams RiskModel
    get_prior_risk = function(pregnancy, g = 37, pnorm = FALSE) {

      intercept <- 54.3637
      sigma     <-  6.8833

      beta_age                  <- -0.206886
      beta_height               <-  0.11711
      beta_afro_caribbean       <- -2.6786
      beta_south_asian          <- -1.129
      beta_chronic_hypertension <- -7.2897
      # beta_SLE_APS              <- -3.0519
      beta_in_vitro             <- -1.6327

      beta_weight               <- -0.0694096
      beta_family_PE            <- -1.7154
      beta_DM                   <- -3.3899

      mu <- intercept +

        ifelse(pregnancy$get("age", truncate_for = "prior_risk") >= 35, beta_age * (pregnancy$get("age", truncate_for = "prior_risk")-35), 0) +
        beta_height * (pregnancy$get("height", truncate_for = "prior_risk") - 164) +
        ifelse(pregnancy$get("race") == "afro-caribbean", beta_afro_caribbean, 0) +
        ifelse(pregnancy$get("race") == "south-asian", beta_south_asian, 0) +
        ifelse(pregnancy$get("chronic_hypertension") == "yes", beta_chronic_hypertension, 0) +
        # ifelse(form_data$sle == 1 && form_data$aps == 1 && model == "2024", beta_SLE_APS, 0) +
        ifelse(pregnancy$get("conception") == "ivf", beta_in_vitro, 0)

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
            beta_interval * (pregnancy$get("previous_interval", truncate_for = "prior_risk")^-1) +
            beta_interval_05 * (pregnancy$get("previous_interval", truncate_for = "prior_risk")^-0.5) +
            beta_previous_ga * (pregnancy$get("previous_ga", truncate_for = "prior_risk") - 24)^2
        }
      }

      if ( pregnancy$get("chronic_hypertension") == "no" ) {
        mu <- mu +
          beta_weight * (pregnancy$get("weight", truncate_for = "prior_risk") - 69) +
          ifelse(pregnancy$get("mother_pe") == "yes", beta_family_PE, 0) +
          ifelse(pregnancy$get("diabetes_type_i") == "yes" || pregnancy$get("diabetes_type_ii") == "yes", beta_DM, 0)
      }

      if (pnorm) {
        r <- pnorm(g, mean = mu, sd = sigma)
      } else {
        r <- dnorm(g, mean = mu, sd = sigma)
      }

      return(r)
    },

    #' @description Refined risk calculation for 2023.
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

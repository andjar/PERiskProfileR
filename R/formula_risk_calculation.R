#' Calculate Expected PlGF Values
#'
#' @description
#' Calculates the expected Placental Growth Factor (PlGF) values based on various maternal
#' characteristics and demographic factors. Implementation based on Appendix S1 from
#' https://obgyn.onlinelibrary.wiley.com/doi/10.1002/uog.19112
#'
#' @param form_data A list containing the following elements:
#'   \itemize{
#'     \item plgf_machine: Integer (1=DELFIA, 2=KRYPTOR, 3=COBAS)
#'     \item ga: Numeric, gestational age in weeks
#'     \item weight: Numeric, maternal weight in kg
#'     \item age: Numeric, maternal age in years
#'     \item race: Integer (1=White, 2=Afro-Caribbean, 3=South Asian, 4=East Asian, 5=Mixed)
#'     \item smoking: Boolean, smoking status
#'     \item diabetes_type_i: Boolean, presence of Type 1 diabetes
#'     \item diabetes_type_ii: Boolean, presence of Type 2 diabetes
#'     \item conception: Integer (3=in vitro fertilization)
#'     \item previous: Boolean, previous pregnancy status
#'   }
#'
#' @return Numeric value representing the expected PlGF value
#' @keywords local
#' @export
get_expected_plgf <- function(form_data) {
  # Appendix S1 from
  # https://obgyn.onlinelibrary.wiley.com/doi/10.1002/uog.19112

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
    ifelse(form_data$plgf_machine == 1, intercept_delfia,  0) +
    ifelse(form_data$plgf_machine == 2, intercept_kryptor, 0) +
    ifelse(form_data$plgf_machine == 3, intercept_cobas,   0) +

    # GA and weight
    beta_ga  * (form_data$ga*7-77) +
    beta_ga2 * (form_data$ga*7-77)^2 +
    beta_weight  * (form_data$weight - 69) +
    beta_weight2 * (form_data$weight - 69)^2 +
    beta_age * (form_data$age - 35) +

    # Ethnicity
    ifelse(form_data$race == 2, beta_afro_caribbean, 0) +
    ifelse(form_data$race == 3, beta_south_asian, 0) +
    ifelse(form_data$race == 4, beta_east_asian, 0) +
    ifelse(form_data$race == 5, beta_mixed, 0) +

    # Smoking, diabetes, conception
    ifelse(form_data$smoking   == 1, beta_smoking, 0) +
    ifelse(form_data$diabetes_type_i == 1, beta_DM_1, 0) +
    ifelse(form_data$diabetes_type_ii == 1, beta_DM_2, 0) +
    ifelse(form_data$conception == 3, beta_in_vitro, 0) +

    # Previous pregnancy WITHOUT preeclampsia
    ifelse(form_data$previous == 1 && form_data$previous == 2, beta_parous_no_PE, 0)

  return(10^mom)
}

#' Calculate PlGF MoM
#'
#' @description
#' Calculates the Multiple of Median (MoM) for Placental Growth Factor (PlGF)
#'
#' @param form_data A list containing:
#'   \itemize{
#'     \item include_plgf: Integer (0=exclude, 1=use provided MoM, 2=calculate MoM)
#'     \item plgf_mom: Numeric, provided PlGF MoM if include_plgf=1
#'     \item plgf: Numeric, raw PlGF value if include_plgf=2
#'   }
#'
#' @return Numeric value representing PlGF MoM or NA if PlGF is excluded
#' @keywords local
#' @export
get_mom_plgf <- function(form_data) {
  if (form_data$include_plgf == 0) {
    mom_to_return <- NA
  } else if (form_data$include_plgf == 1) {
    mom_to_return <- form_data$plgf_mom
  } else if (form_data$include_plgf == 2) {
    mom_to_return <- form_data$plgf / get_expected_plgf(form_data)
  }

  return(mom_to_return)
}

#' Calculate Expected Uterine Artery Pulsatility Index
#'
#' @description
#' Calculates the expected Uterine Artery Pulsatility Index (UtAPI) based on
#' maternal characteristics
#'
#' @param form_data A list containing maternal characteristics including:
#'   \itemize{
#'     \item ga: Numeric, gestational age in weeks
#'     \item weight: Numeric, maternal weight in kg
#'     \item age: Numeric, maternal age in years
#'     \item race: Integer (2=Afro-Caribbean, 4=East Asian, 5=Mixed)
#'     \item diabetes_type_i: Boolean, presence of Type 1 diabetes
#'     \item previous: Boolean, previous pregnancy status
#'     \item previous_pe: Boolean, previous preeclampsia status
#'   }
#'
#' @return Numeric value representing the expected UtAPI
#' @keywords local
#' @export
get_expected_utpi <- function(form_data) {
  intercept    <-  0.264570000
  beta_ga      <- -0.004838365
  beta_weight  <- -0.000874430
  beta_weight2 <-  0.000007330
  beta_age     <- -0.000641750
  beta_afro_caribbean <- 0.021620000
  beta_east_asian <- 0.007630000
  beta_mixed <- 0.011990000
  beta_DM1   <- -0.027490000

  mom <- intercept +
    beta_ga * (form_data$ga*7 - 77) +
    beta_weight  * (form_data$weight - 69) +
    beta_weight2 * (form_data$weight - 69)^2 +
    beta_age * (form_data$age - 35) + # Men blir mer riktig med -10 istedenfor -35?
    ifelse(form_data$race == 2, beta_afro_caribbean, 0) +
    ifelse(form_data$race == 4, beta_east_asian, 0) +
    ifelse(form_data$race == 5, beta_mixed, 0) +
    ifelse(form_data$diabetes_type_i == 1, beta_DM1, 0)

  if (form_data$previous == 1) {
    if (form_data$previous_pe == 1) {
      intercept_parous <- 0.009650000
      mom <- mom + intercept_parous
    } else {
      intercept_parous <- -0.002950000
      mom <- mom + intercept_parous
    }
  }

  return(10^mom)
}

#' Calculate UtAPI MoM
#'
#' @description
#' Calculates the Multiple of Median (MoM) for Uterine Artery Pulsatility Index
#'
#' @param form_data A list containing:
#'   \itemize{
#'     \item utpi: Numeric, measured UtAPI value
#'   }
#'
#' @return Numeric value representing UtAPI MoM
#' @keywords local
#' @export
get_mom_utpi <- function(form_data) {
  return(
    form_data$utpi / get_expected_utpi(form_data)
  )
}

#' Calculate Expected Mean Arterial Pressure
#'
#' @description
#' Calculates the expected Mean Arterial Pressure (MAP) based on maternal
#' characteristics and medical history
#'
#' @param form_data A list containing:
#'   \itemize{
#'     \item ga: Numeric, gestational age in weeks
#'     \item weight: Numeric, maternal weight in kg
#'     \item height: Numeric, maternal height in cm
#'     \item race: Integer (2=Afro-Caribbean)
#'     \item smoking: Boolean, smoking status
#'     \item chronic_hypertension: Boolean, presence of chronic hypertension
#'     \item diabetes_type_i: Boolean, presence of Type 1 diabetes
#'     \item diabetes_type_ii: Boolean, presence of Type 2 diabetes
#'     \item mother_pe: Boolean, maternal history of preeclampsia
#'     \item previous: Boolean, previous pregnancy status
#'     \item previous_pe: Boolean, previous preeclampsia status
#'     \item previous_interval: Numeric, interval since previous pregnancy in years
#'   }
#'
#' @return Numeric value representing the expected MAP
#' @keywords local
#' @export
get_expected_map <- function(form_data) {

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
    beta_ga  * (form_data$ga*7-77) +
    beta_ga2 * (form_data$ga*7-77)^2 +
    beta_weight  * (form_data$weight - 69) +
    beta_weight2 * (form_data$weight - 69)^2 +
    beta_height  * (form_data$height - 164) +
    ifelse(form_data$race == 2, beta_afro_caribbean, 0) +
    ifelse(form_data$smoking == 1, beta_smoking, 0) +
    ifelse(form_data$chronic_hypertension == 1, beta_chronic_hypertension, 0) +
    ifelse(form_data$chronic_hypertension == 1, beta_chronic_hypertension_weight * (form_data$weight - 69), 0) +
    ifelse(form_data$diabetes_type_i == 1 || form_data$diabetes_type_ii == 2, beta_DM, 0) +
    ifelse(form_data$mother_pe == 1, beta_family_PE, 0)

  if (form_data$previous == 1) {
    if (form_data$previous_pe == 1) {
      intercept_parous <- 0.008570000

      mom <- mom +
        intercept_parous

    } else {
      intercept_parous <- -0.006630000
      beta_interval    <- 0.000826390

      mom <- mom +
        intercept_parous +
        beta_interval * (form_data$previous_interval - 2)

    }
  }

  return(10^mom)
}

#' Calculate MAP MoM
#'
#' @description
#' Calculates the Multiple of Median (MoM) for Mean Arterial Pressure
#'
#' @param form_data A list containing:
#'   \itemize{
#'     \item map: Numeric, measured MAP value
#'   }
#'
#' @return Numeric value representing MAP MoM
#' @keywords local
#' @export
get_mom_map <- function(form_data) {
  return(
    form_data$map / get_expected_map(form_data)
  )
}

#' Calculate Prior Risk
#'
#' @description
#' Calculates the prior risk of preeclampsia based on maternal characteristics
#'
#' @param form_data A list containing maternal characteristics and medical history
#' @param g Numeric, gestational age for risk calculation (default: 37)
#' @param pnorm Boolean, whether to return probability from normal distribution (default: FALSE)
#'
#' @return Numeric value representing the prior risk
#' @keywords local
#' @export
get_prior <- function(form_data, g = 37, pnorm = FALSE){

  intercept <- 54.3637
  sigma <- 6.8833

  beta_age <- -0.206886
  beta_height <- 0.11711
  beta_afro_caribbean <- -2.6786
  beta_south_asian <- -1.129
  beta_chronic_hypertension <- -7.2897
  beta_SLE_APS <- -3.0519
  beta_in_vitro <- -1.6327

  beta_weight <- -0.0694096
  beta_family_PE <- -1.7154
  beta_DM <- -3.3899

  mu <- intercept +
    ifelse(form_data$age >= 35, beta_age * (form_data$age-35), 0) +
    beta_height * (form_data$height - 164) +
    ifelse(form_data$race == 2, beta_afro_caribbean, 0) +
    ifelse(form_data$race == 3, beta_south_asian, 0) +
    ifelse(form_data$chronic_hypertension == 1, beta_chronic_hypertension, 0) +
    ifelse(form_data$sle == 1 || form_data$aps == 1, beta_SLE_APS, 0) +
    ifelse(form_data$conception == 3, beta_in_vitro, 0)

  if (form_data$previous == 1) {
    if (form_data$previous_pe == 1) {
      intercept_parity <- -8.1667
      beta_previous_ga <- 0.0271988

      mu <- mu +
        intercept_parity +
        beta_previous_ga * (form_data$previous_ga - 24)^2

    } else {
      intercept_parity <- -4.335
      beta_interval    <- -4.15137651
      beta_interval_05 <- 9.21473572
      beta_previous_ga <- 0.01549673

      mu <- mu +
        intercept_parity +
        beta_interval * form_data$previous_interval^-1 +
        beta_interval * form_data$previous_interval^-0.5 +
        beta_previous_ga * (form_data$previous_ga - 24)^2
    }
  }

  if (form_data$chronic_hypertension != 1) {
    mu <- mu +
      beta_weight * (form_data$weight - 69) +
      ifelse(form_data$mother_pe == 1, beta_family_PE, 0) +
      ifelse(form_data$diabetes_type_i == 1 || form_data$diabetes_type_ii == 1, beta_DM, 0)
  }

  if (pnorm) {
    r <- pnorm(g, mean = mu, sd = sigma)
  } else {
    r <- dnorm(g, mean = mu, sd = sigma)
  }

  return(r)
}

#' Calculate MAP-PI Joint Distribution
#'
#' @description
#' Calculates the joint probability distribution for MAP and PI MoM values
#'
#' @param mom_MAP Numeric, MAP MoM value
#' @param mom_PI Numeric, PI MoM value
#' @param g Numeric vector, gestational ages
#'
#' @return Numeric vector of probability densities
get_p_MAP_PI <- function(mom_MAP, mom_PI, g) {

  correlation_matrix <- matrix(c(1, -0.05133, -0.05133, 1), 2, 2)
  sd_vector <- c(0.03724, 0.12894)
  covariance_matrix <- diag(sd_vector) %*% correlation_matrix %*% diag(sd_vector)

  MAP <- log10(mom_MAP)
  PI  <- log10(mom_PI)

  sapply(g, function(x) {
    mu_MAP <- ifelse(x < 0.09564/0.001824, 0.09564 - 0.001824 * x, 0)
    mu_PI  <- ifelse(x < 0.54453/0.013143, 0.54453 - 0.013143 * x, 0)

    emdbook::dmvnorm(
      c(MAP, PI),
      mu = c(mu_MAP, mu_PI),
      Sigma = covariance_matrix
    )
  })

}

#' Calculate MAP-PI-PlGF Joint Distribution
#'
#' @description
#' Calculates the joint probability distribution for MAP, PI, and PlGF MoM values
#'
#' @param mom_MAP Numeric, MAP MoM value
#' @param mom_PI Numeric, PI MoM value
#' @param mom_PlGF Numeric, PlGF MoM value
#' @param g Numeric vector, gestational ages
#'
#' @return Numeric vector of probability densities
get_p_MAP_PI_PlGF <- function(mom_MAP, mom_PI, mom_PlGF, g) {
  # https://www.nejm.org/doi/suppl/10.1056/NEJMoa1704559/suppl_file/nejmoa1704559_appendix.pdf

  correlation_matrix <- matrix(
    c(
      1, -0.05133, -0.02791,
      -0.05133, 1, -0.15084,
      -0.02791, -0.15084, 1),
    3, 3)
  sd_vector <- c(0.03724, 0.12894, 0.17723)
  covariance_matrix <- diag(sd_vector) %*% correlation_matrix %*% diag(sd_vector)

  MAP   <- log10(mom_MAP)
  PI    <- log10(mom_PI)
  PlGF  <- log10(mom_PlGF)

  sapply(g, function(x) {
    mu_MAP  <- ifelse(x < 0.09564/0.001824, 0.09564 - 0.001824 * x, 0)
    mu_PI   <- ifelse(x < 0.54453/0.013143, 0.54453 - 0.013143 * x, 0)
    mu_PlGF <- ifelse(x < 0.93687/0.021930, -0.93687 + 0.021930 * x, 0)

    emdbook::dmvnorm(
      c(MAP, PI, PlGF),
      mu = c(mu_MAP, mu_PI, mu_PlGF),
      Sigma = covariance_matrix
    )
  })

}

#' Calculate Prior Probability
#'
#' @description
#' Calculates prior probability combining maternal characteristics and biomarker information
#'
#' @param g Numeric vector, gestational ages
#' @param form_data List containing maternal characteristics
#' @param mom_MAP Numeric, MAP MoM value (default: NA)
#' @param mom_PI Numeric, PI MoM value (default: NA)
#' @param mom_PlGF Numeric, PlGF MoM value (default: NA)
#'
#' @return Numeric vector of probability densities
get_p_prior <- function(g, form_data, mom_MAP = NA, mom_PI = NA, mom_PlGF = NA) {
  if (is.na(mom_PlGF)) {
    get_p_MAP_PI(mom_MAP = mom_MAP, mom_PI = mom_PI, g = g) * get_prior(form_data, g = g)
  } else {
    get_p_MAP_PI_PlGF(mom_MAP = mom_MAP, mom_PI = mom_PI, mom_PlGF = mom_PlGF, g = g) * get_prior(form_data, g = g)
  }
}

#' Calculate Formula-Based Risk
#'
#' @description
#' Calculates the overall risk of preeclampsia using the FMF algorithm
#'
#' @param form_data List containing all required maternal characteristics and measurements
#' @param G Numeric, gestational age cutoff for risk calculation (default: 37)
#' @param report_as_text Boolean, whether to return risk as formatted text (default: FALSE)
#'
#' @return List containing:
#'   \itemize{
#'     \item mom_MAP: MAP MoM value
#'     \item mom_PI: PI MoM value
#'     \item mom_PlGF: PlGF MoM value
#'     \item risk_prior: Prior risk value
#'     \item risk: Final calculated risk
#'   }
#' @export
calculate_formula_risk <- function(form_data, G = 37, report_as_text = FALSE) {
  mom_MAP  <- get_mom_map(form_data)
  mom_PI   <- get_mom_utpi(form_data)
  mom_PlGF <- get_mom_plgf(form_data)
  risk_prior <- get_prior(form_data, g = G, pnorm = TRUE)
  risk <- integrate(get_p_prior,
                    lower = 24, upper = G,
                    form_data = form_data,
                    mom_MAP = mom_MAP, mom_PI = mom_PI, mom_PlGF = mom_PlGF
                    )$value / integrate(
                      get_p_prior, lower = 24, upper = Inf,
                      form_data = form_data,
                      mom_MAP = mom_MAP, mom_PI = mom_PI, mom_PlGF = mom_PlGF
                      )$value

  if (report_as_text == TRUE) {
    risk_prior <- round(1/round(risk_prior, 4))
    risk       <- round(1/round(risk, 4))
  }

  return(list(
    mom_MAP = mom_MAP,
    mom_PI = mom_PI,
    mom_PlGF = mom_PlGF,
    risk_prior = risk_prior,
    risk = risk
  ))
}

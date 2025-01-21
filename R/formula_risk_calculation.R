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
get_expected_plgf <- function(form_data, truncate = TRUE) {
  # Appendix S1 from
  # https://obgyn.onlinelibrary.wiley.com/doi/10.1002/uog.19112

  if (truncate) {
    form_data <- truncate_for_mom(form_data)
  }

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

    # Manufacturer ----
    # Intercept: DELFIA Xpress
    ifelse(form_data$plgf_machine == 1, intercept_delfia,  0) +
    # Intercept: BRAHMS KRYPTOR
    ifelse(form_data$plgf_machine == 2, intercept_kryptor, 0) +
    # Intercept: Cobas e411 	1.542535524
    ifelse(form_data$plgf_machine == 3, intercept_cobas,   0) +

    # GA and weight ----
    # Gestational age in days – 77
    beta_ga  * (form_data$biochemical_ga*7 - 77) +
    # (Gestational age in days – 77)^2
    beta_ga2 * (form_data$biochemical_ga*7 - 77)^2 +
    # Weight in kg - 69
    beta_weight  * (form_data$weight - 69) +
    # (Weight in kg - 69)^2
    beta_weight2 * (form_data$weight - 69)^2 +
    # Maternal age in years - 35
    beta_age * (form_data$age - 35) +

    # Ethnicity ----
    # Racial origin: Afro-Caribbean
    ifelse(form_data$race == 2, beta_afro_caribbean, 0) +
    # Racial origin: South Asian
    ifelse(form_data$race == 3, beta_south_asian, 0) +
    # Racial origin: East Asian
    ifelse(form_data$race == 4, beta_east_asian, 0) +
    # Racial origin: Mixed
    ifelse(form_data$race == 5, beta_mixed, 0) +

    # Smoking, diabetes, conception ----
    # Smoker
    ifelse(form_data$smoking == 1, beta_smoking, 0) +
    # Medical history of diabetes mellitus Type 1
    ifelse(form_data$diabetes_type_i == 1, beta_DM_1, 0) +
    # Medical history of diabetes mellitus Type 2 treated with insulin
    ifelse(form_data$diabetes_type_ii == 1 && form_data$diabetes_drugs %in% c(2, 3), beta_DM_2, 0) +
    # In-vitro fertilization
    ifelse(form_data$conception == 3, beta_in_vitro, 0) +

    # Parous with no history of pre-eclampsia
    ifelse(form_data$previous == 1 && form_data$previous_pe == 2, beta_parous_no_PE, 0)

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
get_mom_plgf <- function(form_data, truncation = TRUE) {
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
get_expected_utpi <- function(form_data, truncate = TRUE) {

  if (truncate) {
    form_data <- truncate_for_mom(form_data)
  }

  intercept    <-  0.264570000
  beta_ga      <- -0.004838365
  beta_weight  <- -0.000874430
  beta_weight2 <-  0.000007330
  beta_age     <- -0.000641750
  beta_afro_caribbean <-  0.021620000
  beta_east_asian     <-  0.007630000
  beta_mixed          <-  0.011990000
  beta_DM1            <- -0.027490000

  mom <- intercept +
    beta_ga * (form_data$biophysical_ga*7 - 77) +
    beta_weight  * (form_data$weight - 69) +
    beta_weight2 * (form_data$weight - 69)^2 +
    beta_age * (form_data$age - 35) +
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
get_mom_utpi <- function(form_data, truncation = TRUE) {

  mom_to_return <- form_data$utpi / get_expected_utpi(form_data)

  return(mom_to_return)
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
get_expected_map <- function(form_data, truncate = TRUE) {

  if (truncate) {
    form_data <- truncate_for_mom(form_data)
  }

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
    beta_ga  * (form_data$biophysical_ga*7-77) +
    beta_ga2 * (form_data$biophysical_ga*7-77)^2 +
    beta_weight  * (form_data$weight - 69) +
    beta_weight2 * (form_data$weight - 69)^2 +
    beta_height  * (form_data$height - 164) +
    ifelse(form_data$race == 2, beta_afro_caribbean, 0) +
    ifelse(form_data$smoking == 1, beta_smoking, 0) +
    ifelse(form_data$chronic_hypertension == 1, beta_chronic_hypertension, 0) +
    ifelse(form_data$chronic_hypertension == 1, beta_chronic_hypertension_weight * (form_data$weight - 69), 0) +
    ifelse(form_data$diabetes_type_i == 1, beta_DM, 0) +
    ifelse(form_data$diabetes_type_ii == 1, beta_DM, 0) +
    ifelse(form_data$mother_pe == 1, beta_family_PE, 0)

  if (form_data$previous == 1) {
    if (form_data$previous_pe == 1) {
      intercept_parous <- 0.008570000

      mom <- mom +
        intercept_parous

    } else {
      intercept_parous <- -0.006630000
      beta_interval    <-  0.000826390

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
get_mom_map <- function(form_data, truncation = TRUE) {

  mom_to_return <- form_data$map / get_expected_map(form_data)

  return(mom_to_return)
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
  sigma     <- 6.8833

  beta_age                  <- -0.206886
  beta_height               <- 0.11711
  beta_afro_caribbean       <- -2.6786
  beta_south_asian          <- -1.129
  beta_chronic_hypertension <- -7.2897
  beta_SLE_APS              <- -3.0519
  beta_in_vitro             <- -1.6327

  beta_weight               <- -0.0694096
  beta_family_PE            <- -1.7154
  beta_DM                   <- -3.3899

  form_data <- truncate_for_risk(form_data)

  mu <- intercept +
    ifelse(form_data$age >= 35, beta_age * (form_data$age-35), 0) +
    beta_height * (form_data$height - 164) +
    ifelse(form_data$race == 2, beta_afro_caribbean, 0) +
    ifelse(form_data$race == 3, beta_south_asian, 0) +
    ifelse(form_data$chronic_hypertension == 1, beta_chronic_hypertension, 0) +
    ifelse(form_data$sle == 1 && form_data$aps == 1, beta_SLE_APS, 0) +
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
      beta_interval_05 <-  9.21473572
      beta_previous_ga <-  0.01549673

      mu <- mu +
        intercept_parity +
        beta_interval * (form_data$previous_interval^-1) +
        beta_interval_05 * (form_data$previous_interval^-0.5) +
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

get_covariance_matrix <- function(has_map = TRUE, has_utpi = TRUE, has_plgf = TRUE) {
  # https://www.nejm.org/doi/suppl/10.1056/NEJMoa1704559/suppl_file/nejmoa1704559_appendix.pdf

  # correlation_matrix <- matrix(
  #   c(
  #     1, -0.05133, -0.02791,
  #     -0.05133, 1, -0.15084,
  #     -0.02791, -0.15084, 1),
  #   3, 3)
  # sd_vector <- c(0.03724, 0.12894, 0.17723)
  # covariance_matrix <- diag(sd_vector) %*% correlation_matrix %*% diag(sd_vector)

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

get_mus <- function(mom_MAP, mom_PI, mom_PlGF, x) {
  b0_MAP <-  0.088997
  b1_MAP <- -0.0016711

  b0_PI <-  0.5861
  b1_PI <- -0.014233

  b0_PlGF <- -0.92352
  b1_PlGF <- 0.021584

  # mu_MAP  <- ifelse(x < 0.09564/0.001824, 0.09564 - 0.001824 * x, 0)
  mu_MAP  <- ifelse(x < -b0_MAP/b1_MAP, b0_MAP + b1_MAP * x, 0)
  # mu_PI   <- ifelse(x < 0.54453/0.013143, 0.54453 - 0.013143 * x, 0)
  mu_PI  <- ifelse(x < -b0_PI/b1_PI, b0_PI + b1_PI * x, 0)
  # mu_PlGF <- ifelse(x < 0.93687/0.021930, -0.93687 + 0.021930 * x, 0)
  mu_PlGF <- ifelse(x < -b0_PlGF/b1_PlGF, b0_PlGF + b1_PlGF * x, 0)

  return(
    c(mu_MAP, mu_PI, mu_PlGF)[c(!is.na(mom_MAP), !is.na(mom_PI), !is.na(mom_PlGF))]
  )
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
#' @keywords internal
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
#' @keywords internal
get_p_prior <- function(g, form_data, mom_MAP = NA, mom_PI = NA, mom_PlGF = NA) {
    get_p_MAP_PI_PlGF(mom_MAP = mom_MAP, mom_PI = mom_PI, mom_PlGF = mom_PlGF, g = g) * get_prior(form_data, g = g)
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
calculate_formula_risk <- function(form_data, G = 37, report_as_text = FALSE, mom_MAP = NA, mom_PI = NA, mom_PlGF = NA, truncation_of_mom = TRUE) {

  result <- list()

  if (is.na(mom_MAP)) {
    mom_MAP  <- get_mom_map(form_data)
    result[["mom_MAP"]] <- mom_MAP
  } else {
    result[["mom_MAP"]] <- mom_MAP
  }
  if (truncation_of_mom & !is.na(mom_MAP)) {
    # Truncation
    # https://doi.org/10.1016/j.ajog.2019.11.1247
    upper_limit <- 10^0.12240759
    lower_limit <- 10^-0.1224076
    if (mom_MAP < lower_limit) {
      warning(paste0("The MoM for MAP will be truncated from ", round(mom_MAP, 2), " to ", round(lower_limit, 2)))
      mom_MAP <- max(mom_MAP, lower_limit)
    } else if (mom_MAP > upper_limit) {
      warning(paste0("The MoM for MAP will be truncated from ", round(mom_MAP, 2), " to ", round(upper_limit, 2)))
      mom_MAP <- min(mom_MAP, upper_limit)
    }
  }

  if (is.na(mom_PI)) {
    mom_PI   <- get_mom_utpi(form_data)
    result[["mom_PI"]] <- mom_PI
  } else {
    result[["mom_PI"]] <- mom_PI
  }
  if (truncation_of_mom & !is.na(mom_PI)) {
    # Truncation
    # https://doi.org/10.1016/j.ajog.2019.11.1247
    upper_limit <- 10^0.42161519
    lower_limit <- 10^-0.4216152
    if (mom_PI < lower_limit) {
      warning(paste0("The MoM for UtAPI will be truncated from ", round(mom_PI, 2), " to ", round(lower_limit, 2)))
      mom_PI <- max(mom_PI, lower_limit)
    } else if (mom_PI > upper_limit) {
      warning(paste0("The MoM for UtAPI will be truncated from ", round(mom_PI, 2), " to ", round(lower_limit, 2)))
      mom_PI <- min(mom_PI, upper_limit)
    }
  }

  if (is.na(mom_PlGF)) {
    mom_PlGF <- get_mom_plgf(form_data)
    result[["mom_PlGF"]] <- mom_PlGF
  } else {
    result[["mom_PlGF"]] <- mom_PlGF
  }
  if (truncation_of_mom & !is.na(mom_PlGF)) {
    # Truncation
    # https://doi.org/10.1016/j.ajog.2019.11.1247
    upper_limit <- 10^0.56550992
    lower_limit <- 10^-0.5655099
    if (mom_PlGF < lower_limit) {
      warning(paste0("The MoM for PlGF will be truncated from ", round(mom_PlGF, 2), " to ", round(lower_limit, 2)))
      mom_PlGF <- max(mom_PlGF, lower_limit)
    } else if(mom_PlGF > upper_limit) {
      warning(paste0("The MoM for PlGF will be truncated from ", round(mom_PlGF, 2), " to ", round(upper_limit, 2)))
      mom_PlGF <- min(mom_PlGF, upper_limit)
    }
  }


  risk_prior <- get_prior(form_data, g = G, pnorm = TRUE)
  risk <- integrate(get_p_prior,
                    lower = 24, upper = G,
                    form_data = form_data,
                    mom_MAP = mom_MAP, mom_PI = mom_PI, mom_PlGF = mom_PlGF
                    )$value / integrate(
                      get_p_prior, lower = 24, upper = 1e3,
                      form_data = form_data,
                      mom_MAP = mom_MAP, mom_PI = mom_PI, mom_PlGF = mom_PlGF
                      )$value

  if (report_as_text == TRUE) {
    risk_prior <- risk_to_text(risk_prior)
    risk <- risk_to_text(risk)
  }

  result[["risk_prior"]] <- risk_prior
  result[["risk"]] <- risk

  return(result)
}

truncate_for_mom <- function(form_data) {
  form_data$weight <- min(form_data$weight, 130)
  form_data$previous_interval <- min(form_data$previous_interval, 20)
  return(form_data)
}

truncate_for_risk <- function(form_data) {

  # Truncations
  # https://doi.org/10.1016/j.ajog.2019.11.1247

  if (form_data$age < 12 || form_data$age > 55) {
    warning("Truncation of age (should be 12-55 years)")
    form_data$age <- ifelse(form_data$age < 12, 12, form_data$age)
    form_data$age <- ifelse(form_data$age > 55, 55, form_data$age)
  }

  if (form_data$weight < 34 || form_data$weight > 190) {
    warning("Truncation of weight (should be 34-190 kg)")
    form_data$weight <- ifelse(form_data$weight < 34, 34, form_data$weight)
    form_data$weight <- ifelse(form_data$weight > 190, 190, form_data$weight)
  }

  if (form_data$height < 127 || form_data$height > 198) {
    warning("Truncation of height (should be 127-198 cm)")
    form_data$height <- ifelse(form_data$height < 127, 127, form_data$height)
    form_data$height <- ifelse(form_data$height > 198, 198, form_data$height)
  }

  if (form_data$previous == 1) {
    if (form_data$previous_ga < 24 || form_data$previous_ga > 42) {
      warning("Truncation of previous_ga (should be 24-42 weeks)")
      form_data$previous_ga <- ifelse(form_data$previous_ga < 24, 24, form_data$previous_ga)
      form_data$previous_ga <- ifelse(form_data$previous_ga > 42, 42, form_data$previous_ga)
    }

    if (form_data$previous_interval < 0.25 || form_data$previous_interval > 15) {
      warning("Truncation of previous_interval (should be 0.25-15 years)")
      form_data$previous_interval <- ifelse(form_data$previous_interval < 0.25, 0.25, form_data$previous_interval)
      form_data$previous_interval <- ifelse(form_data$previous_interval > 15, 15, form_data$previous_interval)
    }
  }

  return(form_data)
}

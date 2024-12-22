#' Get Expected Column Names
#' @description Returns the expected column names for PE risk form data
#' @return Character vector of expected column names
get_expected_columns <- function() {
  c(
    # Pregnancy Details
    "twins",     # 1: Singleton, 2: Twins (Monochorionic), 3: Twins (Dichorionic)
    "crl1",      # Crown-rump length for first fetus (mm)
    "crl2",      # Crown-rump length for second fetus (mm), if twins
    "crl",       # crl
    "ga",        # ga in weeks
    "ga_at",     # Examination date (format: "dd-mm-yyyy")

    # Maternal Characteristics
    "dob",       # Date of birth (format: "dd-mm-yyyy")
    "height",    # Height in cm
    "weight",    # Weight in kg
    "race",      # 1: White, 2: Black, 3: South Asian, 4: East Asian, 5: Mixed
    "smoking",   # 1: Yes, 2: No
    "mother_pe", # 1: Yes, 2: No
    "conception", # 1: Spontaneous, 2: Ovulation drugs, 3: In vitro fertilization

    # Medical History
    "chronic_hypertension", # 1: Yes, 2: No
    "diabetes_type_i",      # 1: Yes, 0: No
    "diabetes_type_ii",     # 1: Yes, 0: No
    "diabetes_drugs",       # 1: Diet only, 2: Insulin, 3: Insulin+Metformin, 4: Metformin
    "sle",                  # 1: Yes, 0: No
    "aps",                  # 1: Yes, 0: No

    # Obstetric History
    "previous",             # 0: Nulliparous, 1: Parous
    "previous_pe",          # 1: Yes, 2: No
    "previous_delivered_at", # Date of previous delivery
    "previous_ga_weeks",    # Weeks of gestation at previous delivery
    "previous_ga_days",     # Days of gestation at previous delivery
    "previous_interval",    # Years since previous delivery

    # Biophysical Measurements
    "map",            # Mean Arterial Pressure
    "utpi",           # Mean Uterine Artery Pulsatility Index
    "biophysical_at", # Date of biophysical measurement (format: "dd-mm-yyyy")

    # Biochemical Measurements
    "include_plgf",   # 0: No, 1: MoM, 2: Raw data
    "include_pappa",  # 0: No, 1: MoM, 2: Raw data
    "plgf_mom",       # PLGF in MoM
    "plgf",           # PlGF
    "plgf_machine",   # 1: Delfia, 2: Kryptor, 3: Roche
    "pappa_mom",      # PAPP-A in MoM
    "pappa",          # Pappa
    "pappa_machine",  # 1: Delfia, 2: Kryptor, 3: Roche
    "biochemical_at"  # Date of biochemical measurement (format: "dd-mm-yyyy")
  )
}

#' Validate DataFrame Columns
#' @description Checks if the dataframe has the required columns for PE risk form
#' @param df A dataframe containing preeclampsia risk data
#' @return List with validation results
validate_columns <- function(df) {
  expected_cols  <- get_expected_columns()
  actual_cols    <- colnames(df)

  # Find missing and extra columns
  missing_cols <- setdiff(expected_cols, actual_cols)
  extra_cols   <- setdiff(actual_cols, expected_cols)

  # Return validation results
  list(
    valid = length(missing_cols) == 0,
    missing_columns = missing_cols,
    extra_columns = extra_cols,
    message = if(length(missing_cols) > 0) {
      paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    } else {
      "All required columns present"
    }
  )
}

#' Convert data frame row to PE Risk Form Arguments
#' @description Converts a row from a dataframe into a list of arguments for submit_pe_risk_form
#' @param row A single row from a dataframe containing preeclampsia risk data
#' @param validate Logical indicating whether to validate columns before conversion
#' @return A list of arguments formatted for submit_pe_risk_form
row_to_list <- function(row, validate = TRUE) {
  if (validate) {
    # Convert row to one-row data frame if it's not already
    if (!is.data.frame(row)) {
      row <- as.data.frame(t(row))
    }

    # Validate columns
    validation <- validate_columns(row)
    if (!validation$valid) {
      warning(validation$message)
    }
  }

  # Define the mapping between data frame column names and form parameters
  param_list <- list(
    # Pregnancy Details
    twins = row$twins,             # 1: Singleton, 2: Twins (Monochorionic), 3: Twins (Dichorionic)
    crl1 = row$crl1,               # Crown-rump length for first fetus (mm)
    crl2 = row$crl2,               # Crown-rump length for second fetus (mm), if twins
    crl = row$crl,                 # crl
    ga = row$ga,                   # ga in weeks
    ga_at = row$ga_at,             # Examination date (format: "dd-mm-yyyy")

    # Maternal Characteristics
    dob = row$dob,                 # Date of birth (format: "dd-mm-yyyy")
    height = row$height,           # Height in cm
    weight = row$weight,           # Weight in kg
    race = row$race,               # 1: White, 2: Black, 3: South Asian, 4: East Asian, 5: Mixed
    smoking = row$smoking,         # 1: Yes, 2: No
    mother_pe = row$mother_pe,     # 1: Yes, 2: No
    conception = row$conception,   # 1: Spontaneous, 2: Ovulation drugs, 3: In vitro fertilization

    # Medical History
    chronic_hypertension = row$chronic_hypertension,  # 1: Yes, 2: No
    diabetes_type_i = row$diabetes_type_i,            # 1: Yes, 0: No
    diabetes_type_ii = row$diabetes_type_ii,          # 1: Yes, 0: No
    diabetes_drugs = row$diabetes_drugs,              # 1: Diet only, 2: Insulin, 3: Insulin+Metformin, 4: Metformin
    sle = row$sle,                                    # 1: Yes, 0: No
    aps = row$aps,                                    # 1: Yes, 0: No

    # Obstetric History
    previous = row$previous,                           # 0: Nulliparous, 1: Parous
    previous_pe = row$previous_pe,                     # 1: Yes, 2: No
    previous_delivered_at = row$previous_delivered_at, # Date of previous delivery
    previous_ga_weeks = row$previous_ga_weeks,         # Weeks of gestation at previous delivery
    previous_ga_days = row$previous_ga_days,           # Days of gestation at previous delivery
    previous_interval = row$previous_interval,         # Years since previous delivery

    # Biophysical Measurements
    map = row$map,                       # Mean Arterial Pressure
    utpi = row$utpi,                     # Mean Uterine Artery Pulsatility Index
    biophysical_at = row$biophysical_at, # Date of biophysical measurement (format: "dd-mm-yyyy")

    # Biochemical Measurements
    include_plgf = row$include_plgf,     # 0: No, 1: MoM, 2: Raw data
    include_pappa = row$include_pappa,   # 0: No, 1: MoM, 2: Raw data
    plgf_mom = row$plgf_mom,             # PLGF in MoM
    plgf = row$plgf,                     # PlGF
    plgf_machine = row$plgf_machine,     # 1: Delfia, 2: Kryptor, 3: Roche
    pappa_mom = row$pappa_mom,           # PAPP-A in MoM
    pappa = row$pappa,                   # Pappa
    pappa_machine = row$pappa_machine,   # 1: Delfia, 2: Kryptor, 3: Roche
    biochemical_at = row$biochemical_at  # Date of biochemical measurement (format: "dd-mm-yyyy")
  )

  if (!is.na(param_list$previous_ga_weeks) & !is.na(param_list$previous_ga_days)) {
    param_list[["previous_ga"]] <- previous_ga_weeks + previous_ga_days / 7
  }

  # Calculate maternal age at conception
  param_list$age <- as.numeric(
    difftime(
      as.Date(param_list$ga_at, tryFormats = c("%d.%m.%Y", "%d-%m-%Y", "%Y-%m-%d")) - param_list$ga*7,
      as.Date(param_list$dob, tryFormats = c("%d.%m.%Y", "%d-%m-%Y", "%Y-%m-%d")),
      units = "days"
    )
    ) / 365.25

  # Convert from strings to numerical values
  param_list$twins <- tolower(param_list$twins)
  if(any(!param_list$twins %in% c("no", "monochorionic", "dichorionic"))) {
    warning("Unknown values found in column for Twins!")
  }
  param_list$twins <- which(param_list$twins == c("no", "monochorionic", "dichorionic"))

  param_list$smoking <- tolower(param_list$smoking)
  if(any(!param_list$smoking %in% c("yes", "no"))) {
    warning("Unknown values found in column for Smoking!")
  }
  param_list$smoking <- ifelse(param_list$smoking == "yes", 1, 2)

  param_list$mother_pe <- tolower(param_list$mother_pe)
  if(any(!param_list$mother_pe %in% c("yes", "no"))) {
    warning("Unknown values found in column for Familiy History of PE!")
  }
  param_list$mother_pe <- ifelse(param_list$mother_pe == "yes", 1, 2)

  param_list$previous <- tolower(param_list$previous)
  if(any(!param_list$previous %in% c("yes", "no"))) {
    warning("Unknown values found in column for Previous Pregnancy!")
  }
  param_list$previous <- ifelse(param_list$previous == "yes", 1, 0)

  param_list$chronic_hypertension <- tolower(param_list$chronic_hypertension)
  if(any(!param_list$chronic_hypertension %in% c("yes", "no"))) {
    warning("Unknown values found in column for Chronic Hypertension!")
  }
  param_list$chronic_hypertension <- ifelse(param_list$chronic_hypertension == "yes", 1, 2)

  param_list$diabetes_type_i <- tolower(param_list$diabetes_type_i)
  if(any(!param_list$diabetes_type_i %in% c("yes", "no"))) {
    warning("Unknown values found in column for Diabetes Type I!")
  }
  param_list$diabetes_type_i <- ifelse(param_list$diabetes_type_i == "yes", 1, 0)

  param_list$diabetes_type_ii <- tolower(param_list$diabetes_type_ii)
  if(any(!param_list$diabetes_type_ii %in% c("yes", "no"))) {
    warning("Unknown values found in column for Diabetes Type II!")
  }
  param_list$diabetes_type_ii <- ifelse(param_list$diabetes_type_ii == "yes", 1, 0)

  param_list$diabetes_drugs <- tolower(param_list$diabetes_drugs)
  if (param_list$diabetes_type_i == 1 || param_list$diabetes_type_ii == 1) {
    if(any(!param_list$diabetes_drugs %in% c("diet only", "insulin", "insulin+metformin", "metformin", ""))) {
      warning("Unknown values found in column for Diabetes Drugs!")
    }
    param_list$diabetes_drugs <- which(param_list$diabetes_drugs == c("diet only", "insulin", "insulin+metformin", "metformin"))
  } else {
    param_list$diabetes_drugs <- NA
  }

  param_list$sle <- tolower(param_list$sle)
  if(any(!param_list$sle %in% c("yes", "no"))) {
    warning("Unknown values found in column for SLE!")
  }
  param_list$sle <- ifelse(param_list$sle == "yes", 1, 0)

  param_list$aps <- tolower(param_list$aps)
  if(any(!param_list$aps %in% c("yes", "no"))) {
    warning("Unknown values found in column for APS!")
  }
  param_list$aps <- ifelse(param_list$aps == "yes", 1, 0)

  param_list$include_plgf <- tolower(param_list$include_plgf)
  if(any(!param_list$include_plgf %in% c("no", "mom", "raw"))) {
    warning("Unknown values found in column for PlGF (no, mom, raw)!")
  }
  param_list$include_plgf <- which(param_list$include_plgf == c("no", "mom", "raw")) - 1
  # NB: From 0-2

  if(param_list$include_plgf > 0) {
    param_list$plgf_machine <- tolower(param_list$plgf_machine)
    if(any(!param_list$plgf_machine %in% c("delfia", "kryptor", "roche"))) {
      warning("Unknown values found in column for PlGF Machine!")
    }
    param_list$plgf_machine <- which(param_list$plgf_machine == c("delfia", "kryptor", "roche"))
  }

  param_list$include_pappa <- tolower(param_list$include_pappa)
  if(any(!param_list$include_pappa %in% c("no", "mom", "raw"))) {
    warning("Unknown values found in column for PAPP-A (no, mom, raw)!")
  }
  param_list$include_pappa <- which(param_list$include_pappa == c("no", "mom", "raw")) - 1
  # NB: From 0-2

  if(param_list$include_pappa > 0) {
    param_list$pappa_machine <- tolower(param_list$pappa_machine)
    if(any(!param_list$pappa_machine %in% c("delfia", "kryptor", "roche"))) {
      warning("Unknown values found in column for PAPP-A Machine!")
    }
    param_list$pappa_machine <- which(param_list$pappa_machine == c("delfia", "kryptor", "roche"))
  }

  # Remove NULL values and NA values
  param_list <- param_list[!sapply(param_list, is.null) & !sapply(param_list, is.na)]

  return(param_list)
}

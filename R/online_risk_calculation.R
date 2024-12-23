#' Calculate Online Preeclampsia Risk
#'
#' @description
#' Submits maternal characteristics and measurements to the Fetal Medicine Foundation's
#' online preeclampsia risk calculator and retrieves the results.
#'
#' @param form_data A list containing maternal characteristics and measurements
#' @param save_responses Boolean, whether to save the HTML responses (default: FALSE)
#' @param response_dir String, directory path to save responses (default: "requests")
#' @param url String, FMF calculator URL (default: "https://fetalmedicine.org/research/assess/preeclampsia/First")
#' @param row_id String or NULL, identifier for the response file (default: NULL)
#'
#' @return List containing:
#'   \itemize{
#'     \item status: "Success" or "Error"
#'     \item content: HTML content if successful
#'     \item code: HTTP status code if error
#'     \item message: Error message if applicable
#'   }
#'
#' @importFrom httr POST status_code content http_status
#' @keywords online
#' @export
calculate_online_risk <- function(
  form_data, # See rows_to_list.R
  save_responses = FALSE,
  response_dir = "requests",

  # Form submission URL
  url = "https://fetalmedicine.org/research/assess/preeclampsia/First",
  row_id = NULL

) {

  # Send POST request
  response <- httr::POST(
    url,
    body = map_to_calculator_form(form_data),
    encode = "form"
  )

  # Check response
  if (httr::status_code(response) == 200) {
    content <- httr::content(response, "text", encoding = "UTF-8")
    if (save_responses == TRUE) {
      # Generate unique filename based on twins and ga_at
      filename <- file.path(response_dir, paste0(row_id, "_", format(Sys.time(), "%Y-%m-%dT%H%M%S"), ".html"))

      # Write content to HTML file
      writeLines(content, con = filename)
    }

    return(list(
      status = "Success",
      content = content
    ))
  } else {
    return(list(
      status = "Error",
      code = httr::status_code(response),
      message = httr::http_status(response)$message
    ))
  }
}

#' Map Form Data to Calculator Format
#'
#' @description
#' Converts internal form data format to the format expected by the FMF calculator
#'
#' @param form_data List containing form data in internal format
#'
#' @return List with parameter names mapped to calculator format
#'
#' @details
#' Prepends "CalculatorPeMom[" to each parameter name as required by the FMF calculator
#'
map_to_calculator_form <- function(form_data) {

  mapping <- get_expected_columns()
  param_list <- list()

  for (original_name in mapping) {
    calculator_name <- paste0("CalculatorPeMom[", original_name, "]")
    param_list[[calculator_name]] <- form_data[[original_name]]
  }

  return(param_list)
}

#' Extract Risk Scores from HTML Content
#'
#' @description
#' Extracts preeclampsia risk scores and biomarker MoM values from FMF calculator HTML response
#'
#' @param html_content String containing HTML response from calculator
#' @param report_as_text Boolean, whether to return risks as text fractions (default: FALSE)
#'
#' @return List containing:
#'   \itemize{
#'     \item mom_MAP: Mean Arterial Pressure MoM
#'     \item mom_PI: Pulsatility Index MoM
#'     \item mom_PlGF: PlGF MoM
#'     \item risk_prior: Prior risk (numeric or text)
#'     \item risk: Final risk (numeric or text)
#'   }
#'
#' @importFrom rvest read_html html_nodes html_text
extract_risk_scores <- function(html_content, report_as_text = FALSE) {

  html_content <- rvest::read_html(html_content)

  # Extract risk scores using CSS selectors
  history_risk <- html_content |>
    rvest::html_nodes(".history table td:nth-child(2)") |>
    rvest::html_text() |>
    trimws()

  if (report_as_text == FALSE) {
    history_risk <- gsub("1 in ", "", history_risk)
    history_risk <- 1 / as.numeric(history_risk)
  }

  markers_risk <- html_content |>
    rvest::html_nodes(".markers table td:nth-child(2)") |>
    rvest::html_text() |>
    trimws()

  if (report_as_text == FALSE) {
    markers_risk <- gsub("1 in ", "", markers_risk)
    markers_risk <- 1 / as.numeric(markers_risk)
  }

  mom_MAP_string <- html_content |>
    rvest::html_nodes("#pe-report > div:nth-child(3) > div:nth-child(1)") |>
    rvest::html_text() |>
    trimws()

  mom_MAP <- as.numeric(sub(".*pressure.*?\\((\\d+\\.\\d+)\\s*MoM\\).*", "\\1", mom_MAP_string))
  mom_PI <- as.numeric(sub(".*PI.*?\\((\\d+\\.\\d+)\\s*MoM\\).*", "\\1", mom_MAP_string))

  mom_PlGF_string <- html_content |>
    rvest::html_nodes("#pe-report > div:nth-child(3) > div:nth-child(2)") |>
    rvest::html_text() |>
    trimws()
  mom_PlGF <- as.numeric(sub(".*PLGF\\s*(\\d+\\.\\d+)\\s*MoM.*", "\\1", mom_PlGF_string))

  return(list(
    mom_MAP = mom_MAP,
    mom_PI = mom_PI,
    mom_PlGF = mom_PlGF,
    risk_prior = history_risk,
    risk = markers_risk
  ))
}

#' Extract Risk Scores from HTML File
#'
#' @description
#' Reads an HTML file and extracts preeclampsia risk scores and biomarker MoM values
#'
#' @param html_file String, path to HTML file
#' @param report_as_text Boolean, whether to return risks as text fractions (default: FALSE)
#'
#' @return List containing risk scores and MoM values (see extract_risk_scores)
#'
#' @seealso \code{\link{extract_risk_scores}}
#' @keywords online
#' @export
extract_risk_scores_from_file <- function(html_file, report_as_text = FALSE) {
  extract_risk_scores(html_file, report_as_text = report_as_text)
}

#' Extract Risk Scores from Multiple HTML Files
#'
#' @description
#' Processes multiple HTML response files and combines the extracted risk scores into a data table
#'
#' @param response_dir String, directory containing HTML files (default: "requests")
#' @param report_as_text Boolean, whether to return risks as text fractions (default: FALSE)
#'
#' @return data.table with columns:
#'   \itemize{
#'     \item file: Source file path
#'     \item mom_MAP: Mean Arterial Pressure MoM
#'     \item mom_PI: Pulsatility Index MoM
#'     \item mom_PlGF: PlGF MoM
#'     \item risk_prior: Prior risk
#'     \item risk: Final risk
#'   }
#'
#' @importFrom data.table rbindlist data.table
#' @keywords online
#' @export
extract_risk_scores_from_files <- function(response_dir = "requests", report_as_text = FALSE) {
  files_to_read <- list.files(path = response_dir, pattern = "*.html", full.names = TRUE)
  df <- rbindlist(lapply(files_to_read, function(file_to_read) {
    k <- extract_risk_scores_from_file(file_to_read, report_as_text = report_as_text)
    data.table(
      file = file_to_read,
      mom_MAP = k$mom_MAP,
      mom_PI = k$mom_PI,
      mom_PlGF = k$mom_PlGF,
      risk_prior = k$risk_prior,
      risk = k$risk
    )
  }))
  df
}

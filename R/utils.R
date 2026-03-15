#' Convert Risk Text to Numeric Probability
#'
#' @description
#' Converts a character string in the format "1 in X" into a numeric
#' probability (1/X).
#'
#' @param txt A character string starting with "1 in " followed by a number.
#'
#' @return A numeric value representing the probability.
#'
#' @examples
#' text_to_risk("1 in 100")
#' # Returns 0.01
#'
#' text_to_risk("1 in 50")
#' # Returns 0.02
#'
#' @seealso \code{\link{risk_to_text}} for the inverse operation
#'
#' @export
text_to_risk <- function(txt) {
  txt <- gsub("1 in ", "", txt)
  return(1 / as.numeric(txt))
}

#' Convert Numeric Probability to Risk Text
#'
#' @description
#' Formats a numeric probability into a human-readable "1 in X" string.
#' The function rounds the input to 4 decimal places before calculating the ratio.
#'
#' @param risk A numeric value representing probability (between 0 and 1).
#' @param max_risk A numeric value representing the lowest probability to display.
#'   Values below this threshold are capped. Defaults to \code{1/10000}.
#'
#' @return A character string in "1 in X" format (e.g., "1 in 500").
#'   If the input is infinite or below \code{max_risk}, returns "1 in 10000".
#'
#' @details
#' The function applies double rounding: first to the input \code{risk} at 4 decimal
#' places, and then to the resulting denominator. This mimics the display format
#' used by the FMF online calculator.
#'
#' @examples
#' risk_to_text(0.02)
#' # Returns "1 in 50"
#'
#' risk_to_text(0.0003)
#' # Returns "1 in 3333"
#'
#' risk_to_text(0.00001)
#' # Returns "1 in 10000" (capped at max_risk)
#'
#' @seealso \code{\link{text_to_risk}} for the inverse operation
#'
#' @export
risk_to_text <- function(risk, max_risk = 1/10000) {
  if ( is.infinite(risk) || risk < max_risk ) risk <- max_risk
  txt <- paste0("1 in ", round(1/round(risk, 4)))
  return(txt)
}

#' Generate Package Startup Message
#' @keywords internal
PERiskProfileRStartupMessage <- function() {
  # Source: https://stackoverflow.com/questions/67986577/how-to-create-custom-start-up-messages-for-r-packages
  msg <- c(
    "-------------------------------------------------------------------",
    "PERiskProfileR: Preeclampsia Risk Profiling Tool",
    "-------------------------------------------------------------------",
    "DISCLAIMER: This package is NOT intended for clinical use.",
    "NON-AFFILIATION: This software is independently developed and is ",
    "NOT affiliated with, endorsed by, or supported by the ",
    "Fetal Medicine Foundation (FMF).",
    "LIABILITY: The authors assume no responsibility for clinical ",
    "decisions made based on this software.",
    "-------------------------------------------------------------------"
  )
  return(paste(msg, collapse = "\n"))
}

#' Package Attachment Hook
#'
#' @description
#' This internal function is automatically called when the package is
#' attached (e.g., via \code{library(PERiskProfileR)}). It displays
#' the clinical disclaimer to the console.
#'
#' @param lib Character. The library directory where the package is located.
#' @param pkg Character. The name of the package.
#'
#' @seealso \code{\link[base]{.onAttach}}
#' @keywords internal
.onAttach <- function(lib, pkg)
{
  # startup message
  msg <- PERiskProfileRStartupMessage()
  packageStartupMessage(msg)
  invisible()
}

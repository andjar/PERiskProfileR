text_to_risk <- function(txt) {
  txt <- gsub("1 in ", "", txt)
  return(1 / as.numeric(txt))
}

risk_to_text <- function(risk) {
  if ( is.infinite(risk) ) risk <- 1/10000
  txt <- paste0("1 in ", round(1/round(risk, 4)))
  return(txt)
}

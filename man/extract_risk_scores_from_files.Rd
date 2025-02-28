% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/online_risk_calculation.R
\name{extract_risk_scores_from_files}
\alias{extract_risk_scores_from_files}
\title{Extract Risk Scores from Multiple HTML Files}
\usage{
extract_risk_scores_from_files(
  response_dir = "requests",
  data = NULL,
  id_column = "id",
  report_as_text = FALSE
)
}
\arguments{
\item{response_dir}{String, directory containing HTML files (default: "requests")}

\item{report_as_text}{Boolean, whether to return risks as text fractions (default: FALSE)}
}
\value{
data.table with columns:
\itemize{
\item file: Source file path
\item mom_MAP: Mean Arterial Pressure MoM
\item mom_PI: Pulsatility Index MoM
\item mom_PlGF: PlGF MoM
\item risk_prior: Prior risk
\item risk: Final risk
}
}
\description{
Processes multiple HTML response files and combines the extracted risk scores into a data table
}
\keyword{online}

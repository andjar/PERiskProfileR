% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formula_risk_calculation.R
\name{get_expected_plgf}
\alias{get_expected_plgf}
\title{Calculate Expected PlGF Values}
\usage{
get_expected_plgf(form_data, truncate = TRUE)
}
\arguments{
\item{form_data}{A list containing the following elements:
\itemize{
\item plgf_machine: Integer (1=DELFIA, 2=KRYPTOR, 3=COBAS)
\item ga: Numeric, gestational age in weeks
\item weight: Numeric, maternal weight in kg
\item age: Numeric, maternal age in years
\item race: Integer (1=White, 2=Afro-Caribbean, 3=South Asian, 4=East Asian, 5=Mixed)
\item smoking: Boolean, smoking status
\item diabetes_type_i: Boolean, presence of Type 1 diabetes
\item diabetes_type_ii: Boolean, presence of Type 2 diabetes
\item conception: Integer (3=in vitro fertilization)
\item previous: Boolean, previous pregnancy status
}}
}
\value{
Numeric value representing the expected PlGF value
}
\description{
Calculates the expected Placental Growth Factor (PlGF) values based on various maternal
characteristics and demographic factors. Implementation based on Appendix S1 from
https://obgyn.onlinelibrary.wiley.com/doi/10.1002/uog.19112
}
\keyword{local}

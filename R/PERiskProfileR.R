calculate_risk <- function (df, model = "FMF2023", as_list = TRUE, G = 37) {

  assertChoice(model, c("FMF2023", "FMF2025", "None"))

  if (model == "FMF2023") {
    risk_model <- RiskModelFMFM2023$new(G = G)
  } else if (model == "FMF2025") {
    risk_model <- RiskModelFMFM2025$new(G = G)
  } else if (model == "None") {
    risk_model <- RiskModelEmpty$new(G = G)
  }

  res <- pbapply::pblapply(seq_len(nrow(df)), function(i) {
    Pregnancy$new(
      params = as.list(df[i, ]),
      risk_model = risk_model
    )
  })

  if ( as_list == TRUE ) {
    return(res)
  } else {
    return(as_df(res))
  }

}

as_df <- function(res) {
  rbindlist(
    lapply(res, function(k) k$as_df()),
    fill = TRUE
  )
}

calculate_pe_risk <- function(data,
                              id_column = "id",
                              method = c("formula", "online"),
                              report_as_text = FALSE,
                              G = 37,
                              save_responses = FALSE,
                              response_dir = "requests",
                              url = "https://fetalmedicine.org/research/assess/preeclampsia/First",
                              verbose = TRUE) {

  method <- match.arg(method)

  if(!id_column %in% colnames(data)) {
    data$id <- seq(1, nrow(data))
  }

  # Create response directory if needed
  if (method == "online" && save_responses) {
    dir.create(response_dir, showWarnings = FALSE, recursive = TRUE)
  }

  n_rows <- nrow(data)
  df_out <- copy(data)

  # Process each row
  for (i in seq_len(n_rows)) {
    if (verbose) {
      cat(sprintf("\rProcessing row %d of %d", i, n_rows))
    }

    row_data <- row_to_list(data[i,])

    if (method == "formula") {
      risk_scores <- calculate_formula_risk(row_data, G = G, report_as_text = report_as_text)

      df_out[i, mom_MAP := risk_scores$mom_MAP]
      df_out[i, mom_PI := risk_scores$mom_PI]
      df_out[i, mom_PlGF := risk_scores$mom_PlGF]
      df_out[i, risk_prior := risk_scores$risk_prior]
      df_out[i, risk := risk_scores$risk]
    } else if (method == "online") {
      result <- calculate_online_risk(
        row_data,
        save_responses = save_responses,
        response_dir = response_dir,
        url = url,
        row_id = data[[id_column]][i]
      )

      if (result$status == "Success") {
        risk_scores <- extract_risk_scores(result$content, report_as_text = report_as_text)
        df_out[i, mom_MAP := risk_scores$mom_MAP]
        df_out[i, mom_PI := risk_scores$mom_PI]
        df_out[i, mom_PlGF := risk_scores$mom_PlGF]
        df_out[i, risk_prior := risk_scores$risk_prior]
        df_out[i, risk := risk_scores$risk]
      } else {
        warning(sprintf("Failed to calculate risk for row %d: %s", i, result$message))
      }
    }
  }

  if (verbose) cat("\nCalculations complete.\n")
  return(df_out)
}

calculate_pe_risk <- function(data,
                              id_column = "id",
                              method = c("formula", "online"),
                              report_as_text = FALSE,
                              G = 37,
                              save_responses = FALSE,
                              response_dir = "requests",
                              url = "https://fetalmedicine.org/research/assess/preeclampsia/First",
                              verbose = TRUE,
                              parallel = NA) {

  method <- match.arg(method)
  warning("Please Note: This package is NOT intended for clinical use!")

  # Add ID column if missing
  if (!id_column %in% colnames(data)) {
    data$id <- seq_len(nrow(data))
  }

  # Create response directory if needed
  if (method == "online" && save_responses) {
    dir.create(response_dir, showWarnings = FALSE, recursive = TRUE)
  }

  n_rows <- nrow(data)
  df_out <- copy(data)

  # Helper function for processing a single row
  process_row <- function(row_data, row_id) {
    if (method == "formula") {
      risk_scores <- calculate_formula_risk(row_data, G = G, report_as_text = report_as_text)
    } else {
      result <- calculate_online_risk(
        row_data,
        save_responses = save_responses,
        response_dir = response_dir,
        url = url,
        row_id = row_id
      )
      if (result$status != "Success") {
        warning(sprintf("Failed to calculate risk for row %d: %s", row_id, result$message))
        return(NULL)
      }
      risk_scores <- extract_risk_scores(result$content, report_as_text = report_as_text)
    }
    return(risk_scores)
  }

  if (is.na(parallel)) {
    if (n_rows > 100) {
      parallel <- TRUE
    } else {
      parallel <- FALSE
    }
  }

  # Process data sequentially or in parallel
  if (parallel) {
    if (verbose) message("Using parallel processing for large dataset.")

    # Register parallel backend
    num_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(num_cores)
    doParallel::registerDoParallel(cl)

    results <- foreach(i = seq_len(n_rows), .packages = c("data.table", "PERiskProfileR"), .combine = rbind) %dopar% {
      row_data <- PERiskProfileR::row_to_list(data[i,])
      risk_scores <- as.data.table(process_row(row_data, data[[id_column]][i]))
      if (!is.null(risk_scores)) {
        cbind(data[i,], risk_scores)
      } else {
        data[i,]  # Return original row on failure
      }
    }

    stopCluster(cl)
    df_out <- rbind(results, fill = TRUE)
  } else {
    if (verbose) message("Using sequential processing for small dataset.")

    for (i in seq_len(n_rows)) {
      if (verbose) cat(sprintf("\rProcessing row %d of %d", i, n_rows))

      row_data <- PERiskProfileR::row_to_list(data[i,])
      risk_scores <- process_row(row_data, data[[id_column]][i])

      if (!is.null(risk_scores)) {
        df_out[i, mom_MAP := risk_scores$mom_MAP]
        df_out[i, mom_PI := risk_scores$mom_PI]
        df_out[i, mom_PlGF := risk_scores$mom_PlGF]
        df_out[i, risk_prior := risk_scores$risk_prior]
        df_out[i, risk := risk_scores$risk]
      }
    }

    if (verbose) cat("\nCalculations complete.\n")
  }

  return(df_out)
}

#' Retrieve Demo Dataset
#'
#' This function provides access to a demo dataset included in the package.
#' The dataset is stored as a CSV file in the `extdata` directory of the package.
#'
#' @return A `data.table` object containing the contents of the demo dataset.
#'
#' @examples
#' # Load the demo dataset
#' demo_data <- get_demo_data()
#' head(demo_data)
#'
#' @importFrom data.table fread
#' @export
get_demo_data <- function() {
  return(fread(system.file("extdata", "demo.csv", package = "PERiskProfileR")))
}

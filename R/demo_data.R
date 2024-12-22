get_demo_data <- function() {
  return(fread(system.file("extdata", "demo.csv", package = "PERiskProfileR")))
}

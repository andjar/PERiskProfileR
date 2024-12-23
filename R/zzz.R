PERiskProfileRStartupMessage <- function()
{
  #https://stackoverflow.com/questions/67986577/how-to-create-custom-start-up-messages-for-r-packages
  msg <- c("PLEASE NOTE: This package is NOT intended for clinical use")
  return(msg)
}

.onAttach <- function(lib, pkg)
{
  # startup message
  msg <- PERiskProfileRStartupMessage()
  packageStartupMessage(msg)
  invisible()
}

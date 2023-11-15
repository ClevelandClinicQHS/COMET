.pkgenv <- new.env(parent=emptyenv())
.onLoad <- function(libname, pkgname) {
  pres_data <- requireNamespace("cometdata", quietly = TRUE)
  .pkgenv[["pres_data"]] <- pres_data
}
.onAttach <- function(libname, pkgname) {
  if (!.pkgenv$pres_data) {
    msg <- paste("To use parts of this package, you must install the",
                  "cometdata package. To install that",
                  "package, run 'remotes::install_github('https://github.com/ClevelandClinicQHS/cometdata', type = 'source')'."
                 # "See the 'COMET' readme for more details."
                 )
    msg <- paste(strwrap(msg, width = 100), collapse="\n")
    packageStartupMessage(msg)
  }
}
data_present <- function(pres_data = .pkgenv$pres_data) {
  if (!pres_data) {
    msg <- paste("To use this function, you must have the",
                  "'cometdata' package installed. See the",
                  "'COMET' package readme for more details.")
    msg <- paste(strwrap(msg, width = 72), collapse="\n")
    stop(msg)
  }
}

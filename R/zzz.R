.pkgenv <- new.env(parent=emptyenv())
.onLoad <- function(libname, pkgname) {
  pres_data <- requireNamespace("cometdata", quietly = TRUE)
  .pkgenv[["pres_data"]] <- pres_data
}
.onAttach <- function(libname, pkgname, prompt = NULL) {
  if (!.pkgenv$pres_data) {
    msg <- paste("To use parts of this package, you must install the",
                  "'cometdata' package. To install that",
                  "package, run 'remotes::install_github('https://github.com/ClevelandClinicQHS/cometdata', type = 'source')'."
                 # "See the 'COMET' readme for more details."
                 )
    msg <- paste(strwrap(msg, width = 100), collapse="\n")
    cat(msg)
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

# install_cometdata <- function(prompt = NULL){
#   prompt <- if (isTRUE(getOption('knitr.in.progress'))) "n"
#   while (is.null(prompt)) {
#     cat("To use parts of this package, you must install the 'cometdata' package.\nTo install type 1 and press enter or run\n'remotes::install_github('https://github.com/ClevelandClinicQHS/cometdata', type = 'source')'\nPress any other key to continue")
#     prompt <- tolower(readLines(con = stdin(), n = 1L))
#   }
#   if(prompt == 1){
#     cat('cometdata will be installed. You may need to restart R after the installation is complete.')
#     remotes::install_github('https://github.com/ClevelandClinicQHS/cometdata', type = 'source')
#   }else{
#     stop("the package 'cometdata' will not be installed.\nYou will need to install this package to run `run_simulation` and other parts of COMET", call. = FALSE)
#   }
# }

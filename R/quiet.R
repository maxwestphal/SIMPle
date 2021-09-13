# +++ copied from package ddpcr +++
#' @importFrom utils capture.output
quiet <- function (expr, all = TRUE)
{
  if (Sys.info()["sysname"] == "Windows") {
    file <- "NUL"
  }
  else {
    file <- "/dev/null"
  }
  if (all) {
    suppressWarnings(suppressMessages(suppressPackageStartupMessages(utils::capture.output(expr,
                                                                                           file = file))))
  }
  else {
    utils::capture.output(expr, file = file)
  }
}

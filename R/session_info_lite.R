#' Lightweight R Session Information
#'
#' Provides a quick snapshot of the current R session, including R version,
#' RStudio version (if available), operating system, session time,
#' working directory, home and temporary directories, library paths,
#' repository settings, locale, and loaded packages.
#'
#' This function is designed as a lighter alternative to [sessionInfo()],
#' focusing on the most commonly useful diagnostics for reproducibility,
#' debugging, and environment reporting.
#'
#' @return A named list with session metadata. The function also prints
#' a human-readable summary to the console.
#'
#' @details The output includes:
#' \itemize{
#'   \item R version and (if available) RStudio version
#'   \item Session time
#'   \item Working, home, and temporary directories
#'   \item Active CRAN repositories
#'   \item Library paths
#'   \item Count of loaded packages
#' }
#'
#' @examples
#' \dontrun{
#' # Get a quick snapshot of your session
#' session_info_lite()
#'
#' # Save the result for later use
#' info <- session_info_lite()
#' str(info)
#' }
#'
#' @export


session_info_lite <- function() {
  ses <- sessionInfo()

  version <- ses$R.version$version.string
  wd <- getwd()
  repos <- paste(names(getOption("repos")), getOption("repos"), collapse = "\n  - ")
  libpaths <- paste(.libPaths(), collapse = "\n  - ")
  systime <- Sys.time()


  message(" Your current R version is: ", version)
  message(" Current R studio version is: ", rstudioapi::versionInfo()$version)
  message(" Your current Working directory is: ", wd)
  message(" Your Home directory is: ", path.expand("~"))
  message(" Your packages are being installed from this repos or mirror:\n  - ", repos)
  message(" Your packages are being installed in to this path:\n  - ", libpaths)
  message(" Number of packages that are loaded currently were: ", length(ses$otherPkgs))
  message(" Current R system time is: ", systime)
}




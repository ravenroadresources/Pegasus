#' Makes Pegasus fly
#'
#' This function is a wrapup to run the Pegasus shinyapp.
#'    It takes no arguments.
#'
#' @importFrom shiny runApp
#' @importFrom utils browseURL
#'
#' @examples
#' \dontrun{
#' ROFM::run()
#' }
#' @export
fly <- function() {
  require(Pegasus)
  package_path <- path.package("Pegasus")
  shiny_path <- file.path(package_path, "shiny")
  shiny::runApp(shiny_path)
}
